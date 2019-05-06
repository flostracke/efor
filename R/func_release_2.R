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
