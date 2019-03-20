# -- Function for applying prophet to multiple articles

# -- external functions ----

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
tf_prophet_grid <- function(data, test_data, n_pred, freq, parallel = FALSE, parameter_grid, ...) {

  #INIT Columns for rmse
  parameter_grid$rmse <- NA

  #Iterate over all passed parameter combinations
  # Search best parameters. Rewrite with furrr::map=
  for (i in 1:nrow(parameter_grid)) {

    parameters <- parameter_grid[i, ]

    forecasts <-
      tf_grouped_forecasts(
        data,
        n_pred,
        prophet::prophet,
        freq,
        parallel,
        #specific prophet parameters
        seasonality.prior.scale = parameters$seasonality.prior.scale,
        changepoint.prior.scale = parameters$changepoint.prior.scale,
        changepoint.range = parameters$changepoint.range,
        seasonality.mode = parameters$seasonality.mode,
        ...
      )

    calced_rmse <- dplyr::inner_join(test_data, forecasts, by = c("iterate", "date")) %>%
      yardstick::rmse(y.x, y.y) %>%
      pull(.estimate)

    parameter_grid$rmse[i] <- calced_rmse
  }
  return(parameter_grid)
}