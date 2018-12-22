# Functions for applying Boxcox transformations.

# -- exported functions ----

#' @title Get lambdas used in Boxcox Transformation.
#'
#' @description Gets the lambdas relevant for a boxcox transformation. The
#' lambdas are fitted on the data. The lambdas are returned for multiple
#' timeseries in a dataframe, if there is a column "iterate".
#'
#' @param data The data which the lambdas are obtained for. Needs a column
#'             "iterate" as an unique id.
#'
#' @return Returns the lambdas for all the timeseries in the dataframe.
#' @export
#'
#' @examples tf_get_lambdas(salesdata::sales_monthly)
#'
tf_get_lambdas <- function(data) {

  #check input data for correct structure
  check_input_data(data)

  #get lambda for each series in the tibble
  lambdas <- data %>% split(.$iterate) %>%
    purrr::map_df(tidy_bc_lambda)

  return(lambdas)
}

#' Removes the boxcox transformation applied to multiple timeseries.
#'
#' @param data The data with the boxcox transformation applied to.
#' @param lambdas The data frame with the lambdas. Can be created with
#'                \code{\link{tf_get_lambdas}}
#'
#' @return Returns the series with the boxcox transformation removed.
#' @export
#'
#' @examples lambdas <- tf_get_lambdas(salesdata::sales_monthly)
#'           boxcox <- salesdata::sales_monthly %>%
#'           tf_boxcox()
#'           tf_remove_boxcox(boxcox, lambdas)
#'
tf_remove_boxcox <- function(data, lambdas) {

  #check input data for correct structure
  check_input_data(data)

  # remove the transformation for each series in the tibble
  removed_boxcox <- data %>%
    split(.$iterate) %>%
    purrr::map_df(remove_box_cox, lambdas)

  return(removed_boxcox)
}

#' Applies the boxcox transformation to multiple timeseries.
#'
#' @param data The data which should be transformed.
#'
#' @return The data with boxcox transformed values.
#' @export
#'
#' @examples tf_boxcox(salesdata::sales_monthly)
#'
tf_boxcox <- function(data) {

  #check input data for correct structure
  check_input_data(data)

  # apply the transformation to each series in the tibble.
  boxcoxed_data <- data %>%
    split(.$iterate) %>%
    purrr::map_df(tidy_bc)

  return(boxcoxed_data)
}

# -- Internal Functions ----

#-- Applies the boxcox transformation to one series. This is an internal function.
tidy_bc <- function(data) {

  #check input data for correct structure
  check_input_data(data)

  # get the vector with the values which should be transformed
  values <- data$y

  # get the used lambda for the transfromation
  lambda <- forecast::BoxCox.lambda(values)

  # apply boxcox function from the forecast package with the fitted lambda
  boxcox_trans <- forecast::BoxCox(values, lambda)

  # add the transformed values to a dataframe
  df_boxcoc <- dplyr::mutate(data, y = boxcox_trans)

  return(df_boxcoc)
}

# Returns a dataframe with the id of the series and the learned lambda.
# This is an internal function
tidy_bc_lambda <- function (data) {

  #check input data for correct structure
  check_input_data(data)

  # get the vector with the values which should be transformed
  values <- data$y

  # get the used lambda for the transfromation
  lambda <- forecast::BoxCox.lambda(values)

  # build a tibble with the id of the series and the lambda value
  lambda_result <- tibble::tibble(
    iterate = unique(data$iterate),
    lambda = lambda
  )

  return(lambda_result)
}

# removes the box cox transformation.
# This is an internal function.
remove_box_cox <- function(data, lambda) {

  #check input data for correct structure
  check_input_data(data)

  # get the vector with the values which should be retransformed
  values <- data$y

  # get the needed lambda value from the lambda dataframe
  used_lambda <- dplyr::filter(lambda, iterate == unique(data$iterate)) %>%
    dplyr::pull(lambda)

  # remove the transformation with the corresponding lambda
  removed_boxcox <- forecast::InvBoxCox(values, used_lambda)

  # add the retransformed value to a tibble
  inversed_forecasts <- dplyr::mutate(data, y = removed_boxcox)

  return(inversed_forecasts)
}
