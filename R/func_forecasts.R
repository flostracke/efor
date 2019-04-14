# -- Functions for applying timeseries methods to multiple articles

# -- external functions ----

#' Creates forecast for a dateframe.
#'
#' The Dataframe has to have the columny date and y. Following methods are
#' supported:
#'
#' - all methods form the forecasts package
#' - prophet
#' - forecastHybrid
#' - forecastxgb
#' - smooth package
#'
#' @param data The dataframe containing the timeseries.
#' @param n_pred The forecast horizon.
#' @param func The used forecast method.
#' @param freq The used frequence. 1 for daily data and 12 for monthly data.
#' @param ... More arguments specific to the used forecasting method.
#'
#' @export
#'
#' @return The forecasted values for the series

tf_forecast <- function(data, n_pred, func, freq, ...) {

  # get the name of the used function
  name <- find_original_name(func)

  # Forecasts with the prophet model:
  if (name == "prophet") {

    preds <- tf_prophet(data, n_pred, freq,  ...)

    return(preds)

  } else {
    # Forecasts from the forecast package

    # Create the model object
    mod <- tf_create_model(data, func, freq, ...)

    #save the current iterate to a string
    current_iterate <- unique(data$iterate)

    preds <-
      # create the forecasts with the trained model
      build_forecasts(mod, n_pred, freq) %>%
      # add iterate column
      mutate(iterate = current_iterate) %>%
      # add name of forecasting method
      mutate(key = name) %>%
      #select columns for output with predictions intervals
      select(date, iterate, key, y, y_lo.95, y_hi.95)

    return(preds)
  }
}

#' Creates forecast for a dateframe with mutlitple timeseries separted by the
#' iterate column.
#'
#' The Dataframe has to have the columny date, iterate and y. Following methods are
#' supported:
#'
#' - all methods form the forecasts package
#' - prophet
#' - forecastHybrid
#' - forecastxgb
#' - smooth package
#'
#' The forecasts are created in parallel with the help of the furrr package.
#' Since the package is quite fresh there might be some bugs involved
#' (for example the prophet model is somehow not working in parallel mode).
#'
#' @param data The dataframe containing the timeseries.
#' @param n_pred The forecast horizon.
#' @param func The used forecast method.
#' @param freq The used frequency. 1 for daily data and 12 for monthly data.
#' @param parallel Specifies if the forecasts are created in parallel.
#' @param ... More arguments specific to the used forecasting method.
#'
#' @export
#'
#' @return The forecasted values for the dataset.
tf_grouped_forecasts <- function(data, n_pred, func, freq, parallel = T, ...) {

  create_plan()

  if(parallel == F) {
    # Prophet funktioniert nicht mit future map.. nur purr map nutzen!
    data %>%
      split(.$iterate) %>%
      purrr::map(tf_forecast, n_pred, func, freq, ...) %>%
      dplyr::bind_rows()
  } else {
    data %>%
      split(.$iterate) %>%
      furrr::future_map(tf_forecast, n_pred, func, freq, ...) %>%
      dplyr::bind_rows()
  }

}

#' Returns a simple mean forecast for each iterate
#'
#' @param data A tibble object
#' @param h The number of future dates that should be forecasted
#'
#' @return The tibble with the mean forecasts
#' @export
#'
#' @examples tf_mean_forecast(sales_monthly, h = 2)
tf_mean_forecast <- function(data, h) {

  #get the mean values for each iterate
  value_mean <-  dplyr::group_by(data, iterate) %>%
    dplyr::summarise(y = mean(y)) %>%
    dplyr::ungroup() %>%
    #for simulating a cross join
    dplyr::mutate(dummy = 1)

  #construct the dataframe with the future values
  date <- timetk::tk_make_future_timeseries(idx = unique(data$date), n_future = h)

  dates <- tibble::tibble(date) %>%
    # for cross join
    dplyr::mutate(dummy = 1)

  #construct the final tibble
  mean_forecasts <- dplyr::inner_join(value_mean, dates, by = c("dummy")) %>%
    dplyr::select(-dummy) %>%
    dplyr::mutate(key = "mean_forecast")

  return(mean_forecasts)
}


# -- internal functions ----

#' Builds forecasts with the trained model.
#'
#' @param mod The trained model.
#' @param n_pred The number of predictions
#' @param freq The frequency. Currently only daily data (1) and monthly data (12).
#'             are supported.
#'
#' @return The predictions with nice date representations
#'
#' @examples
build_forecasts <- function(mod, n_pred, freq) {

  # Create the forecast
  preds <- forecast_and_sweep(mod, n_pred)

  # now we have to rebuild a nice time representation

  # Create Forecasts for Monthly data
  if(freq == 12) {
    preds <- preds %>%
      dplyr::mutate(date = tf_yearmon_to_date(date))

  } else {

    # save the original timetk index from the model
    index <- timetk::tk_index(mod, timetk_idx = T)

    # build the dates which should be forecasted
    future_dates <- timetk::tk_make_future_timeseries(index, n_pred)

    preds <- preds %>%
      dplyr::mutate(date = future_dates)

  }

  return(preds)
}

#' Actually calls the forecast functions and applies sweep to the result.
#'
#' @param mod The pretrained model.
#' @param n_pred The number of predictions
#'
#' @return The produced forecast. The date column has to be corrected.
#'
#' @examples
forecast_and_sweep <- function(mod, n_pred) {
  forecast <- forecast::forecast(mod, n_pred) %>%
    sweep::sw_sweep(fitted = F, rename_index = "date") %>%
    # filter out the history of the series
    dplyr::filter(key != "actual") %>%
    dplyr::select(
      date,
      key,
      y,
      y_lo.95 = lo.95,
      y_hi.95 = hi.95
    )
  return(forecast)
}

#' Creates models based on ts objects.
#'
#' Helper function that creates models based on ts objects. For that the passed
#' dataframe is casted to a ts objeckt with the help of the timetk package.
#' You can use following methods with this function:
#'
#' - all methods form the forecasts package
#' - forecastHybrid
#' - forecastxgb
#' - smooth package
#'
#' @param data The dataframe containing the timeseries.
#' @param func The used forecast method.
#' @param freq The used frequence. 1 for daily data and 12 for monthly data.
#' @param ... More arguments specific to the used forecasting method.
#'
#' @return The fitted model for the specified method.

tf_create_model <- function(data, func, freq, ...) {

  # get the start date for timetk depending on daily or monthly data
  start_date <- tf_start_date(data, freq)


  # Coerce the dataframe to a ts object
  ts_data <- data %>%
    timetk::tk_ts(start = start_date,
                  frequency = freq,
                  select = y,
                  silent = T
                  )

  # Construct the model object with additional parameters
  mod <- func(ts_data, ...)

  return(mod)
}










