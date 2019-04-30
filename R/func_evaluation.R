# -- Different functions for measuring performance

# external functions ----

#' Creates the metrics for mutliple models.
#'
#' This function calculates the rmse for all contained forecasts in a dataframe.
#' The different models have to be seperated with the help of the key column.
#'
#' @param df_forecasts The dataframe containing the forecasts.
#' @param df_test The dataframe containing the testset.
#' @param detailed If True the rmse for each article will be returned.
#' @param metrics A list with the returned metrics. Following metrics are
#'                possible:
#'
#'                - rmse
#'                - mae
#'                - rsq
#'                - mase
#'                - mape
#'
#' @examples tf_calc_metrics(sales_forecast, sales_test)
#'
#' @export
#'
#' @return The calculated rmse for each method.
#'
tf_calc_metrics <- function(df_forecasts, df_test, metrics = c("rmse", "mae", "rsq", "mase", "mape"), detailed = F) {



  # calculate rmse, mae and rsq
  res_metrics <- calc_yardstick_metrics(df_forecasts, df_test, func = yardstick::metrics, detailed)

  #calculate mase
  mase <- calc_yardstick_metrics(df_forecasts, df_test, func = yardstick::mase, detailed)

  #calculate mape
  mape <- calc_yardstick_metrics(df_forecasts, df_test, func = yardstick::mape, detailed)

  res_metrics <- res_metrics %>%
    dplyr::bind_rows(mase) %>%
    dplyr::bind_rows(mape) %>%
    dplyr::filter(metric %in% metrics)

  return(res_metrics)
}





#' Counts how often each method is the best method against a testset.
#'
#' @param forecasts The dataframe with all the forecasts included
#' @param testset The testset for counting the best method
#' @param metric The specified metric for choosing the best method
#' @param plot Plots the count distribution.
#'
#' @return The dataframe with the information how many times each method has been the best method
#' @export
#'
#' @examples tf_count_best_method(sales_forecast, sales_test, metric = "rmse", plot = FALSE)
tf_count_best_method <- function(forecasts, testset, metric = "rmse", plot = FALSE) {


  dtl_metrics <- choose_dtl_metric(forecasts, testset, metric)

  #get the minimum rmse for each iterate
  min_metrics <- dtl_metrics %>%
    dplyr::group_by(iterate) %>%
    dplyr::summarise(value = min(value))

  #now innerjoin the min rmse against the rmse of all methods and count how ofteh each method is the best
  count_best_method <- dtl_metrics %>%
    dplyr::inner_join(min_metrics, by = c("iterate", "value")) %>%
    dplyr::count(key)

  if(plot == TRUE) {

    count_best_method %>%
      ggplot2::ggplot(ggplot2::aes(reorder(key, n), n)) +
      ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "How often is each method the best forecast?",
        x = "used forecasting method",
        y = "# is the best method"
      )

  } else {
    return(count_best_method)
  }
}

#' Chooses the best method against a validation set from multiple forecasts
#'
#' @param forecasts_valid The forecasts against the validation set
#' @param valid_set The validation set
#' @param metric The metric which should be used for obtaining the bestmodel
#'
#' @return A dataframe
#' @export
#'
#' @examples tf_get_best_method(sales_forecast, sales_test, metric = "rmse")
tf_get_best_method <- function(forecasts_valid, valid_set, metric = "rmse") {

  #get list with all available models and apply a rank to them in order to
  #resolve cases when it's not clear which is the best model

  models_row_numers <- forecasts_valid %>%
    dplyr::select(key) %>%
    dplyr::distinct() %>%
    dplyr::arrange(key) %>%
    dplyr::mutate(n = row_number())

  #calculate the metric for all models and iterates
  dtl_metric <- choose_dtl_metric(forecasts_valid,valid_set, metric)

  #get the best metric for each iterate
  best_metric <- dtl_metric %>%
    dplyr::group_by(iterate) %>%
    dplyr::summarise(value = min(value))

  #get the info which produces the best forecast
  best_method <- best_metric %>%
    dplyr::inner_join(dtl_metric, by = c("iterate", "value")) %>%
    dplyr::select(iterate, key)

  #add index of the model for choosing always the best model
  best_method <- best_method %>%
    dplyr::left_join(models_row_numers, by = "key")

  #get the list which model should be used for which article
  min_best_method <- best_method %>%
    dplyr::group_by(iterate) %>%
    dplyr::summarise(n = min(n))

  #make the model / iterate combinaion unqiue
  final_best_method <- best_method %>%
    dplyr::inner_join(min_best_method, by = c("iterate", "n"))

  return(final_best_method)

}







# -- Internal Functions ----

#' The functions calcualtes one specific metric for all iterates.
#'
#' @param forecasts The forecast dataframe
#' @param testset The testset dataframe
#' @param metric The metric which should be returned
#'
#' @return The calculated values for all iterates
#'
#'
choose_dtl_metric <- function(forecasts, testset, metric = "rmse") {

  metric_var <- rlang::enquo(metric)

  dtl_metric <- tf_calc_metrics(forecasts, testset, detailed = TRUE) %>%
    dplyr::filter(metric == !!metric_var)

  return(dtl_metric)
}


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




