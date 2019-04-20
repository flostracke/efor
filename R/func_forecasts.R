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
#' @param ... More arguments specific to the used forecasting method.
#'
#' @export
#'
#' @return The forecasted values for the series

tf_forecast <- function(data, n_pred, func, ...) {

  # get the name of the used function
  name <- find_original_name(func)

  # Forecasts with the prophet model:
  if (name == "prophet") {

    preds <- forecasts_prophet(data, n_pred,  ...)


  } else {
  # Forecasts from the forecast package


    preds <- forecasts_timeseries(data, n_pred = n_pred, func = func, name = name)

  }

  return(preds)
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
#' @param parallel Specifies if the forecasts are created in parallel.
#' @param ... More arguments specific to the used forecasting method.
#'
#' @export
#'
#' @return The forecasted values for the dataset.
tf_grouped_forecasts <- function(data, n_pred, func, parallel = T, ...) {

  #get a vector with the original date format. workaounrd for bug in
  #bind_rows, which looses the original date column data type
  #orig_future_dates <- build_final_date_vector(data, n_pred)
  orig_future_dates <- build_final_date_vector(data, n_pred)

  #create plan for multiprocessing
  create_plan()

  if(parallel == F) {

    # Prophet funktioniert nicht mit future map.. nur purr map nutzen!
    forecasts <- data %>%
      split(.$iterate) %>%
      purrr::map(tf_forecast, n_pred, func, ...) %>%
      dplyr::bind_rows()

  } else {

    forecasts <- data %>%
      split(.$iterate) %>%
      furrr::future_map(tf_forecast, n_pred, func, ...) %>%
      dplyr::bind_rows()
  }

  #add the original date colmntye
  forecasts <- dplyr::mutate(forecasts, date = orig_future_dates)

  return(forecasts)

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

#' Actually calls the forecast functions and applies sweep to the result.
#'
#' @param mod The pretrained model.
#' @param n_pred The number of predictions
#'
#' @return The produced forecast. The date column has to be corrected.
#'
#' @examples
create_forecast <- function(data, mod, n_pred) {

  #save the current iterate to a string
  current_iterate <- unique(data$iterate)

  # get a vector with the dates for which the forecasts should be created
  forecasted_dates <- create_forecasting_dates(data, n_pred)

  forecast <-
    # produce the forecats with the passed model object
    forecast::forecast(mod, n_pred) %>%
    # clean the forecasts for the final output
    sweep::sw_sweep(fitted = FALSE, rename_index = "date") %>%
    # filter out the history of the series
    dplyr::filter(key == "forecast") %>%
    # keep only specific interval information
    dplyr::select(
      date,
      key,
      y = value,
      y_lo.95 = lo.95,
      y_hi.95 = hi.95
    ) %>%
    #prevents hw from producing too many forecasts
    head(n_pred) %>%
    # replace the future dates with a nice representation and add the
    # information for the current iterate
    dplyr::mutate(
      date = forecasted_dates,
      iterate = current_iterate
    )

  # returns the dataframe with the forecasts for the current iterate
  return(forecast)
}

#' Creates a vector with the dates for which the forecasts should be created
#'
#' @param data The original dataset
#' @param n_pred number of the forecasts that should be produced
#'
#' @return The vector with the dates of the forecast horizon
#'
#' @examples
create_forecasting_dates <- function(data, n_pred) {

  data %>%
    tsibble::append_row(n = n_pred) %>%
    tail(n_pred) %>%
    dplyr::pull(date)
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
#' @param ... More arguments specific to the used forecasting method.
#'
#' @return The fitted model for the specified method.

tf_create_model <- function(data, func, ...) {

  # convert a single tsibble timeseries to a ts object for passing to the methods
  # from the forecast package
  ts_data <- data %>%
   as.ts()

  # Construct the model object with additional parameters
  mod <- func(ts_data, ...)

  #return the trained model
  return(mod)
}

#' Wrapper function for forecasts from the forecast package
#'
#' @param data The dataset with all the articles
#' @param func The used forecast function
#' @param n_pred Number of the produced forecasts
#' @param ... FUrther arguments which should be passed to the forecast function
#'
#' @return The produced forecasts
#'
#' @examples
forecasts_timeseries <- function(data, func, n_pred, name, ...) {

  # Create the model object
  mod <- tf_create_model(data, func, ...)


  preds <-
    # create the forecasts with the trained model
    create_forecast(data, mod, n_pred) %>%
    # add name of forecasting method
    dplyr::mutate(key = name) %>%
    #select columns for output with predictions intervals
    dplyr::select(date, iterate, key, y, y_lo.95, y_hi.95)

  return(preds)
}

#Functions build the final date vector for the returned forecast dataframe
build_final_date_vector <- function(data, n_pred) {

  #get the number
  nr_iterates <- get_unique_iterates(data) %>%
    length()

  #recycle the future date vector for each included iterate
  orig_future_dates <- data %>%
    tsibble::append_row(n = n_pred) %>%
    tail(n_pred) %>%
    dplyr::pull(date) %>%
    rep(nr_iterates)

  return(orig_future_dates)

}









