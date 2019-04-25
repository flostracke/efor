# Functions for applying and removing log transformation

# external functions ----

#' This functions applies a log transformation to multiple timeseries. There
#' has to be column y in the dataset.
#'
#' @param data The dataset on which the log transformation is applied to. The
#'             values in the y column will be transformed
#' @param constant Before transforming there is a value added to the data. This
#'                 makes it possible to transform 0 values.
#' @param ... Further arguments passed tp the log function.
#'
#' @return Return the log transformed data.
#' @export
#'
#' @examples tf_log(sales_monthly)
tf_log <- function(data, constant = 0.00001, ...) {

  #check input data for correct structure
  check_input_data(data)

  # verify there are no negative values. This ensure that log won't produce Inf
  # values
  if (min(data$y) < 0) stop("There are negative values in y!")

  data %>%
    dplyr::mutate(y = log(y + constant, ...))
}


#' This function rempves
#'
#' @param data The dataset on which the log transformation is removed. The
#'             values in the y column will be transformed
#' @param constant Before applying the exp() function the constant will be
#'                 removed
#'
#' @return The dataset with the removed log transformation.
#' @export
#'
#' @examples sales_monthly %>%
#'            tf_log() %>%
#'            tf_remove_log()
#'
tf_remove_log <- function(data, constant = 0.00001) {

  #check input data for correct structure
  check_input_data(data)

  data %>%
    dplyr::mutate(y = exp(y) - constant)
}
