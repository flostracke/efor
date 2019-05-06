# -- Function for applying prophet to multiple articles

# -- internal functions ----

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
#'
#' @return The forecasted values for the series
forecasts_prophet <- function(data, n_pred, freq, ...) {

  #create the future values for the final result dataframe
  future_dates <- tibble(ds = create_forecasting_dates(data, n_pred))

  #save the current iterate
  current_iterate <- unique(data$iterate)

  #bring the current dataframe to the structure for prophet
  data <- data %>%
    dplyr::select(ds = date, y)

  #train the prophet model
  mod_prophet <- prophet::prophet(data, interval.width = 0.95, ...)


  forecast <-
    # create the prophet forcasts; suppressWarnings against some strange warning message
    suppressWarnings(stats::predict(mod_prophet, future_dates)) %>%

    # wrange the prophet forecasts
    dplyr::mutate(
      key = "prophet",
      iterate = current_iterate
    ) %>%
    dplyr::select(
      date = ds,
      iterate, key,
      y = yhat,
      y_lo_95 = yhat_lower,
      y_hi_95 = yhat_upper
    ) %>%
    mutate(date = dplyr::pull(future_dates))

  #return the wrangeld prophet forecasts
  return(forecast)
}


