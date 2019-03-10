# -- Different functions for measuring performance ----

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
    dplyr::rename( metric = .metric, value = .estimate) %>%
    dplyr::select(-.estimator) %>%
    dplyr::arrange(metric, value)

  return(metrics)
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
#' @examples
tf_count_best_method <- function(forecasts, testset, metric = "rmse", plot = FALSE) {


  metric_var <- rlang::enquo(metric)

  dtl_metrics <- tf_calc_metrics(forecasts, testset, detailed = TRUE) %>%
    dplyr::filter(metric == !!metric_var)

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
      ggplot2::ggplot(aes(reorder(key, n), n)) +
      ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "How often is each method the best forecast?",
        x = "used forecasting method",
        y = "# is the best method"
      ) +
      tidyquant::theme_tq()

  } else {
    return(count_best_method)
  }
}



#' Plots the generated forecast against the actual values for each method. The
#' different forecast methods are grouped by a column "key".
#'
#' @param forecasts The created forecasts for each method
#' @param test The testset with the actual values
#'
#'
#' @export
#'
#' @examples
tf_plot_preds_actuals <- function(forecasts, test) {

  #verifies the input has the correct format
  check_input_data(forecasts)
  check_input_data(test)

  #prepare the labels with ordered rmse
  rmse_labels <- tf_calc_metrics(forecasts, test) %>%
    dplyr::filter(metric == "rmse") %>%
    dplyr::select(key, rmse = value) %>%
    dplyr::mutate(rmse = as.integer(rmse)) %>%
    dplyr::mutate(rmse_order = rmse) %>%
    dplyr::mutate(rmse = as.character(rmse)) %>%
    dplyr::mutate(rmse = stringr::str_c(" (", rmse)) %>%
    dplyr::mutate(rmse = stringr::str_c(rmse, ")")) %>%
    dplyr::mutate(new_label = stringr::str_c(key, rmse)) %>%
    #reorder factors for plotting ascending rmse
    dplyr::mutate(new_label = as_factor(new_label)) %>%
    dplyr::mutate(new_label = forcats::fct_reorder(new_label, rmse_order)) %>%
    dplyr::select(-rmse, -rmse_order)



  dplyr::left_join(forecasts, test, by = c("date", "iterate")) %>%
    dplyr::select(date, key, iterate, forecasts = y.x, actuals = y.y) %>%
    dplyr::left_join(rmse_labels, by = c("key")) %>%
    ggplot2::ggplot(aes(actuals, forecasts)) +
    ggplot2::geom_point(alpha = 0.1) +
    ggplot2::facet_wrap(~new_label) +
    ggplot2::geom_abline(slope = 1L, intercept = 0L, size = 1.1, color = "#56B4E9") +
    ggplot2::ggtitle("Forecasts vs Actuals") +
    ggplot2::labs(
      title = "Forecasts vs Actuals",
      subtitle = "per used method with RMSE"
    ) +
    tidyquant::theme_tq()
}
