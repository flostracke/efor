# -- Functions for decomposition

#' Function calls anomalize::time_decompose() internal and decomposes all
#' series in the tibble
#'
#' @param data A tibble object
#' @param ... Possible parameters of the anomalize::time_decompose() function
#'            Please refer to this function for more details.
#'
#' @return The decomposed timeseries in the tibble object
#' @export
#'
#' @examples tf_decompose(salesdata::sales_monthly)
tf_decompose <- function(data, ...) {

  data %>%
    split(.$iterate) %>%
    purrr::map_df(tidy_decompose, ...)
}

# -- internal helper functions ----

# Functions decomposes ONE timeseries
tidy_decompose <- function(data, ... ) {

  #verify correct data structure
  check_input_data(data)

  data %>%
    #convert to tibbletime for time_decompose function
    tibbletime::as_tbl_time(index = date) %>%
    anomalize::time_decompose(target = "y", merge = T,  ...) %>%
    tibble::as.tibble() %>%
    dplyr::select(-observed)

}

