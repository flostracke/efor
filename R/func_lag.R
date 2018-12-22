# Functions for creating lagged variables for timeseries

#' Function can create
#'
#' @param data The dataframe which should be extended with lagged variables
#' @param lag_var The column name which should be lagged. You can add a vector
#'                for lagging multiple variables at once
#' @param n_lags The number of lags that should be created. You can pass a
#'               squence of natural numbers for creating multiple lags at once
#'
#' @return The dataframe with the added lagged variables
#' @export
#'
#' @examples tf_create_lags(
#'             salesdata::sales_monthly,
#'             lag_var = y,
#'             n_lags = 1:6
#'           )
tf_create_lags <- function(data, lag_var = y, n_lags = 1L:12L) {

  # Verify input data is in the correct format
  check_input_data(data)

  # verifiy n_lags are correct
  check_n_lags(n_lags)

  #enquote for being to able to pass bare column names
  lag_var <- rlang::enquo(lag_var)
  lag_var_name <- rlang::quo_text(lag_var)

  #column Names of lagged variables. this creates a vector of all included lags
  col_names <- stringr::str_c(lag_var_name, "lag", n_lags, sep = "_") %>%
    # unqoting adds some strange ~.. remove it
    stringr::str_replace("~", "")


  # this creates a vector with the variable name and the function call for
  # creating the relevant lag value
  lag_functions <- stats::setNames(paste("dplyr::lag(., ", n_lags, ")"), col_names)

  data %>%
    # split for appling the function to each timeseries separately
    split(.$iterate) %>%
    # create for each time series the lagged values
    purrr::map_df(dplyr::mutate_at, dplyr::vars(!!lag_var), dplyr::funs_(lag_functions))

}


# -- internal helper function ----

# -- function for checking the lagged variables are integers
check_n_lags <- function(n_lags) {

  if(class(n_lags) != "integer") {
    stop("n_lags are no integers!")
  }
}



