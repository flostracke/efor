# -- Functions for applying tsclean

# -- external functions ----

#' Function for applying the forecast::tsclean function
#'
#' Uses supsmu for non-seasonal series and a robust STL decomposition for seasonal #' #' series. To estimate missing values and outlier replacements, linear interpolation #' is used on the (possibly seasonally adjusted) series
#'
#' @param data The dataframe containing a single timeseries
#' @param freq The used frequence. 1 for daily data and 12 for monthly data.
#' @param ... Further parameters passed to the forecast::tsclean function
#'
#' @export
#'
#' @return The cleaned timeseries in a tibble
#'
tf_clean_ts <- function(data, freq, ...) {

  #saves the current iterate
  current_iterate <- unique(data$iterate)

  data %>%
    #cast to ts object
    timetk::tk_ts(start = tf_start_date(., freq), frequency = freq, silent = T) %>%
    forecast::tsclean(...) %>%
    # make negative observations positive
    abs() %>%
    # cast to tibble
    timetk::tk_tbl(rename_index = "date") %>%
    # cast yearmon date column to normale date column
    dplyr::mutate(date = tf_yearmon_to_date(date)) %>%
    #add the current iterate information
    dplyr::mutate(iterate = current_iterate) %>%
    #arrange the columns
    dplyr::select(date, iterate, y)
}

#'
#' TSCLEAN for multiple articles
#'
#' Applie the tsclean function to multiple articles in a dataframe.
#'
#' @param data The dataframe with the timeseries
#' @param parallel If False the function is not executed in parallel
#' @param ... Further arguments that are passed to tsclen
#'
#' @export
#'
#' @return Returns the cleaned dataframe
#'
tf_clean_grouped_ts <- function(data, parallel = T, freq = 12, ...) {

  # create a plan for future execution
  create_plan(parallel)

  data %>%
    split(.$iterate) %>%
    furrr::future_map(tf_clean_ts, .progress = T, freq, ...) %>%
    dplyr::bind_rows()

}


