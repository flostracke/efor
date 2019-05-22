# Functions for creating features from the timestamp


#' The function creates additional features from the date column. You can get
#' a stripped down version for monthly data with type = "monthly
#'
#' @param data The used inpud data
#' @param type Can be "all" or "monthly". With type = "montly" you get a reduced
#'             number of features for monthly data
#'
#' @return The dataframe with the additional features
#' @export
#'
#' @examples sales_monthly %>%
#'             tf_add_time_features(type = "monthly")
tf_add_time_features <- function(data, type = "all") {

  #verifies data is in correct format
  check_input_data(data)

  #verifies type arguemtn is all or monthly
  if(type != "all" & type != "monthly") {
    stop("Unknown type argument")
  }

  # if type equals all, all the possible time features are returned
  if(type == "all") {

    result <- timetk::tk_augment_timeseries_signature(data)
    return(result)
  }

  # if type equals monthly, only some relevant features for monthly data are
  # returend
  if(type == "monthly") {

    # build a df with the relevant time features
    date_features <- data %>%
      dplyr::select(date, iterate) %>%
      timetk::tk_augment_timeseries_signature() %>%
      dplyr::select(date, iterate, year, half, quarter, month)

    # join the time features to the original dataset
    result <- dplyr::left_join(data, date_features, by = c("date", "iterate"))
    return(result)
  }
}

