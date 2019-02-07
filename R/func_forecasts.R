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

  name <- find_original_name(func)

  if (name == "prophet") {
    preds <- tf_prophet(data, n_pred, freq,  ...)

    return(preds)

  } else {
    mod <- tf_create_model(data, func, freq, ...)

    current_iterate <- unique(data$iterate)

    preds <- build_forecasts(mod, n_pred, freq) %>%
      # add iterate column
      mutate(iterate = current_iterate) %>%
      # add name of forecasting method
      mutate(key = name)

    return(preds)
  }
}


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
#' @export
#'
#' @examples
forecast_and_sweep <- function(mod, n_pred) {
  forecast <- forecast::forecast(mod, n_pred) %>%
    sweep::sw_sweep(fitted = F, rename_index = "date") %>%
    # filter out the history of the series
    dplyr::filter(key != "actual") %>%
    dplyr::select(date, key, y)
  return(forecast)
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

#' Creates forecast with prophet for a dateframe.
#'
#' The Dataframe has to have the columny date and y. This method is for creating
#' forecasts with the prophet model.
#'
#' @param data The dataframe containing the timeseries.
#' @param n_pred The forecast horizon.
#' @param freq The used frequency. 1 for daily data and 12 for monthly data.
#' @param ... More arguments that are passed to the prophet model.
#'
#' @export
#'
#' @return The forecasted values for the series
tf_prophet <- function(data, n_pred, freq, ...) {

  # Set frequency for prophet
  if(freq == "12") {
    used_freq <- "m"
  } else {
    used_freq <- "d"
  }

  current_iterate = unique(data$iterate)

  data <- data %>%
    dplyr::rename(ds = date) %>%
    dplyr::select(ds, y)

  mod_prophet <- prophet::prophet(data, interval.width = 0.95, ...)

  future <- prophet::make_future_dataframe(mod_prophet,
                                  periods = n_pred,
                                  freq = used_freq,
                                  include_history = FALSE)

  forecast <- stats::predict(mod_prophet, future) %>%
    dplyr::mutate(key = "prophet",
           iterate = current_iterate) %>%
    dplyr::select(date = ds, key, y = yhat, iterate) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(date = as.Date(date))

  return(forecast)
}

#' Gridseach for Prophetmodels.
#'
#' Implements gridsearch for prophet models over multiple articles.
#'
#' @param data The Dataframe containing the data
#' @param n_pred The forecast horizon
#' @param parallel specifies if the forecasting is run in parallel
#' @param parameter_grid the used parameters in the random search
#'
#' @export
#'
#' @return A dataframe with the rmse for each parameter combination
#'
tf_prophet_grid <- function(data, test_data, n_pred, freq, parallel = F, parameter_grid) {

  #Iterate over all passed parameter combinations
  # Search best parameters
  for (i in 1:nrow(parameter_grid)) {

    parameters = parameter_grid[i, ]
    error = c()

    forecasts = tf_grouped_forecasts(data,
                                     n_pred,
                                     prophet,
                                     freq,
                                     parallel,
                                     growth = 'linear',
                                     seasonality.prior.scale = parameters$seaslity_prior_scale,
                                     changepoint.prior.scale = parameters$changepoint_prior_scale,
                                     n.changepoints = parameters$n_changepoints,
                                     weekly.seasonality = F,
                                     daily.seasonality = F)

    calced_rmse <- dplyr::inner_join(test_data, forecasts, by = c("iterate", "date")) %>%
      yardstick::rmse(y.x, y.y)

    parameter_grid$Value[i] = calced_rmse
  }
  return(parameter_grid)
}

#' Creates the metrics for mutliple models.
#'
#' This function calculates the rmse for all contained forecasts in a dataframe.
#' The different models have to be seperated with the help of the key column.
#'
#' @param df_forecasts The dataframe containing the forecasts.
#' @param df_test The dataframe containing the testset.
#' @param detailed If True the rmse for each article will be returned.
#'
#' @export
#'
#' @return The calculated rmse for each method.
#'
tf_calc_metrics <- function(df_forecasts, df_test, detailed = F) {
  # -- calculate the rmse per method

  # Make the grouping columns to symbols. Then they can be unquoted in the
  # group by statement
  if(detailed) {
    group <- rlang::syms(c("key", "iterate"))
  } else {
    group <- rlang::sym("key")
  }

  metrics <- df_forecasts %>%
    dplyr::select(date, key, iterate, y_hat = y) %>%
    dplyr::inner_join(df_test, by = c("date", "iterate")) %>%
    dplyr::group_by(!!!group) %>%
    tidyr::nest() %>%
    dplyr::mutate(metrics = purrr::map(data,
                              ~yardstick::metrics(
                                data = .x,
                                truth = y,
                                estimate = y_hat
                              ))) %>%
    tidyr::unnest(metrics) %>%
    select(key, metric = .metric, -.estimator, value = .estimate) %>%
    arrange(metric, value)

  return(metrics)
}

#' Function for applying the forecast::tsclean function
#'
#' Uses supsmu for non-seasonal series and a robust STL decomposition for seasonal #' #' series. To estimate missing values and outlier replacements, linear interpolation #' is used on the (possibly seasonally adjusted) series
#'
#' @param data The dataframe containing a single timeseries
#' @param freq The used frequence. 1 for daily data and 12 for monthly data.
#' @param ... Further parameters passed to the forecast::tsclean function
#'
#' @export
#'
#' @return The cleaned timeseries in a tibble
#'
tf_clean_ts <- function(data, freq, ...) {

  #saves the current iterate
  current_iterate <- unique(data$iterate)

  data %>%
    #cast to ts object
    timetk::tk_ts(start = tf_start_date(.), frequency = freq, silent = T) %>%
    forecast::tsclean(...) %>%
    # make negative observations positive
    abs() %>%
    # cast to tibble
    timetk::tk_tbl(rename_index = "date") %>%
    # cast yearmon date column to normale date column
    dplyr::mutate(date = tf_yearmon_to_date(date)) %>%
    #add the current iterate information
    dplyr::mutate(iterate = current_iterate) %>%
    #arrange the columns
    dplyr::select(date, iterate, y)
}

#'
#' TSCLEAN for multiple articles
#'
#' Applie the tsclean function to multiple articles in a dataframe.
#'
#' @param data The dataframe with the timeseries
#' @param parallel If False the function is not executed in parallel
#' @param ... Further arguments that are passed to tsclen
#'
#' @export
#'
#' @return Returns the cleaned dataframe
#'
tf_clean_grouped_ts <- function(data, parallel = T, ...) {

  # create a plan for future execution
  create_plan(parallel)

  data %>%
    split(.$iterate) %>%
    furrr::future_map(tf_clean_ts, .progress = T, ...) %>%
    dplyr::bind_rows()

}


#' Returns a simple mean forecast for each iterate
#'
#' @param data A tibble object
#' @param h The number of future dates that should be forecasted
#'
#' @return The tibble with the mean forecasts
#' @export
#'
#' @examples tf_mean_forecast(salesdata::sales_monthly, h = 2)
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




