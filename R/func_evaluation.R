# -- Different functions for measuring performance

# external functions ----

#' Calculates different metrics for multiple models.
#'
#' This function calculates different metrics for all contained forecasts in a dataframe.
#' The different models have to be seperated with the help of the key column.
#'
#' The metrics are calculated with the help of the yardstick package
#' (\url{https://github.com/tidymodels/yardstick}).
#'
#' @param forecasts The dataframe containing the forecasts.
#' @param testset The dataframe containing the testset.
#' @param detailed If True the metrics for each article will be returned.
#' @param metrics A list with the returned metrics. Following metrics are
#'                possible:
#'
#'                \itemize{
#'                \item {RMSE} Root Mean Squared Error
#'                \item {MAE} Mean Absolute Error
#'                \item {RSQ} R Squared
#'                \item {MASE} Mean Absolute Scaled Error
#'                \item {MAPE} Mean Absolute Percent Error
#'                }
#'
#'
#' @examples tf_calc_metrics(sales_forecast, sales_test)
#'
#' @export
#'
#' @return The calculated metrics for each method.
#'
tf_calc_metrics <- function(forecasts, testset, metrics = c("rmse", "mae", "rsq", "mase", "mape"), detailed = FALSE) {

  #TODO only calculate the metrics contrained in the metrics parameter

  # calculate rmse, mae and rsq
  res_metrics <- calc_yardstick_metrics(forecasts, testset, func = yardstick::metrics, detailed)

  #calculate mase
  mase <- calc_yardstick_metrics(forecasts, testset, func = yardstick::mase, detailed)

  #calculate mape
  mape <- calc_yardstick_metrics(forecasts, testset, func = yardstick::mape, detailed)

  res_metrics <- res_metrics %>%
    dplyr::bind_rows(mase) %>%
    dplyr::bind_rows(mape) %>%
    dplyr::filter(metric %in% metrics)

  return(res_metrics)
}


# -- Internal Functions ----

#'  wrapper for calculating metrics with yardstick functions
#'
#' It is used in \code{\link{tf_calc_metrics}} for making the function calls for
#' different metrics modular.
#'
#'
#' @param forecasts The dataframe containing the forecasts.
#' @param testset The dataframe containing the testset.
#' @param func The yardstick function call.
#' @param detailed If True the metrics for each article will be returned.
#'
#' @return The calculated metrics for each method.
#'
#' @examples \dontrun{calc_yardstick_metrics(forecasts, testset, func = yardstick::mase, detailed)}
calc_yardstick_metrics <- function(forecasts, testset, func, detailed) {

  # Make the grouping columns to symbols. Then they can be unquoted in the
  # group by statement
  if(detailed) {
    group <- rlang::syms(c("key", "iterate"))
  } else {
    group <- rlang::sym("key")
  }

  forecasts %>%
    dplyr::select(date, key, iterate, y_hat = y) %>%
    dplyr::inner_join(testset, by = c("date", "iterate")) %>%
    dplyr::group_by(!!!group) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      metrics = purrr::map(
        data,
        ~func(
          data = .x,
          truth = y,
          estimate = y_hat
        )
      )
    ) %>%
    tidyr::unnest(metrics) %>%
    dplyr::rename( metric = .metric, value = .estimate) %>%
    dplyr::select(-.estimator) %>%
    dplyr::arrange(metric, value) %>%
    dplyr::select(key, metric, value, dplyr::contains("iterate"))

}




