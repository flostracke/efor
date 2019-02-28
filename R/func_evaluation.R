# -- Different functions for measuring performance ----

#' Plots the generated forecast against the actual values for each method. The
#' different forecast methods are grouped by a column "key".
#'
#' @param forecasts The created forecasts for each method
#' @param test The testset with the actual values
#'
#'
#' @export
#'
#' @examples tf_plot_preds_actuals(forecasts, test)
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
    dplyr::mutate(new_label = forcats::forecafct_reorder(new_label, rmse_order)) %>%
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
