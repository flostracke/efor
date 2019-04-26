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




