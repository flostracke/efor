# Functions for creating some plots ----

#' The functions plots the n best or worst forecasts for a given forecasting
#' method
#'
#' @param train_data
#' @param test_data
#' @param forecast_data
#' @param relevant_metric
#' @param n
#' @param top_or_flop
#'
#' @return
#' @export
#'
#' @examples
tf_plot_n_forecasts <- function(train_data, test_data, forecast_data, relevant_metric = "rmse", n = 3L, top_or_flop = "flop") {

  #TODO check the arguments
  #TODO Check for only one forecast method!

  # calculate the metric for each iterate on order to get the top or flop iterates
  dtl_metric <- tf_calc_metrics(
    forecasts_prophet,
    test_data,
    metrics = relevant_metric,
    detailed = TRUE
  ) %>%
    dplyr::filter(metric == relevant_metric)

  #verify if we have to use the head or tail function and set it accordingly
  if(top_or_flop == "top") {
    func <- head
  }

  if(top_or_flop == "flop") {
    func <- tail
  }

  # get the top or flop n iterates
  relevant_iterates <-  dtl_metric %>%
    #Make it dynamic to get the best or the worst article  -> if else with head / tail
    func(n) %>%
    dplyr::pull(iterate)

  #prepare the labels with the ordered metric for the facet
  labels <- dtl_metric %>%
    filter(metric == relevant_metric) %>%
    dplyr::select(iterate, value) %>%
    dplyr::rename(metric = value) %>%
    dplyr::mutate(metric_order = metric) %>%
    dplyr::mutate(metric = as.character(round(metric, 2))) %>%
    dplyr::mutate(metric = stringr::str_c(" (", metric)) %>%
    dplyr::mutate(metric = stringr::str_c(metric, ")")) %>%
    dplyr::mutate(new_label = stringr::str_c(iterate, metric)) %>%
    #reorder factors for plotting ascending rmse
    dplyr::mutate(new_label = forcats::as_factor(new_label)) %>%
    dplyr::mutate(new_label = forcats::fct_reorder(new_label, metric_order )) %>%
    dplyr::select(-metric, -metric_order)

  #prepare the datasets for plotting
  train_data_plot <- train_data %>%
    filter(iterate %in% relevant_iterates) %>%
    dplyr::mutate(key = "train")%>%
    dplyr::left_join(labels, by = c("iterate"))

  test_data_plot <- test_data %>%
    filter(iterate %in% relevant_iterates) %>%
    dplyr::mutate(key = "test")%>%
    dplyr::left_join(labels, by = c("iterate"))

  forecasts_plot <- forecast_data %>%
    dplyr::filter(iterate %in% relevant_iterates) %>%
    dplyr::left_join(labels, by = c("iterate"))

  #save original date column because of bug in bind_rows loosing the date format
  orig_dates <- c(train_data_plot$date, test_data_plot$date, forecasts_plot$date)

  #get the model name
  model_name <- unique(forecasts_plot$key)

  #get the number of iterates
  n_iterates <- as.character(n)

  if(top_or_flop == "flop") {
    internal_top_or_flop <- "worst"
  } else {
    internal_top_or_flop <- "best"
  }

  #build title and subtitle of the plot
  output_title <- stringr::str_c("The ", internal_top_or_flop, " ", n_iterates, " Forecasts from the ", model_name, " model")
  output_subtitle <- stringr::str_c("regarding the ", relevant_metric, " metric")

  bind_rows(train_data_plot, test_data_plot) %>%
    bind_rows(forecasts_plot) %>%
    mutate(date = orig_dates)  %>%
    #TODO capsule plotting in new function!
    ggplot(aes(x = date, y = y, color = key)) +
    geom_point() +
    geom_line() +
    ggplot2::facet_wrap(~new_label, scales = "free") +
    geom_ribbon(
      data = forecasts_plot,
      aes(ymin = y_lo_95, ymax = y_hi_95),
      fill = "blue",
      alpha = 0.2,
      colour = NA
    ) +
    labs(
      title = output_title,
      subtitle = output_subtitle
    ) +
    theme_minimal()

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
#' @examples tf_plot_preds_actuals(sales_forecast, sales_test)
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
    dplyr::mutate(new_label = forcats::as_factor(new_label)) %>%
    dplyr::mutate(new_label = forcats::fct_reorder(new_label, rmse_order)) %>%
    dplyr::select(-rmse, -rmse_order)



  dplyr::left_join(forecasts, test, by = c("date", "iterate")) %>%
    dplyr::select(date, key, iterate, forecasts = y.x, actuals = y.y) %>%
    dplyr::left_join(rmse_labels, by = c("key")) %>%
    ggplot2::ggplot(ggplot2::aes(actuals, forecasts)) +
    ggplot2::geom_point(alpha = 0.1) +
    ggplot2::facet_wrap(~new_label) +
    ggplot2::geom_abline(slope = 1L, intercept = 0L, size = 1.1, color = "#56B4E9") +
    ggplot2::ggtitle("Forecasts vs Actuals") +
    ggplot2::labs(
      title = "Forecasts vs Actuals",
      subtitle = "per used method with RMSE"
    )
}

#' Plots the distribtuion of the residuals of each method
#'
#' @param testset The testset with the acutal values
#' @param forecasts The dataset with the forecasted values
#'
#' @return
#' @export
#'
#' @examples tf_plot_residuals(sales_forecast, sales_test)
tf_plot_residuals <- function(forecasts, testset) {

  forecasts %>%
    dplyr::select(date, key, iterate,  y_hat = y) %>%
    dplyr::inner_join(testset, by = c("date","iterate")) %>%
    dplyr::mutate(residuals = y - y_hat) %>%
    ggplot2::ggplot(ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(~key)
  ggplot2::geom_rug(alpha = 0.2)
}
