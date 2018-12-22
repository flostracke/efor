# -- Different utility functions ----

# -- Get unique iterates


#' Function returns a vector with the unique iterates in a dataset
#'
#' @param data The analysed dataset
#'
#' @return The vector with the unique iterates
#' @export
#'
#' @examples get_unique_iterates(salesdata::sales_daily)
#'
get_unique_iterates <- function(data) {

  # check for correct input data
  check_input_data(data)

  data %>%
    dplyr::select(iterate) %>%
    dplyr::distinct() %>%
    dplyr::pull()
}

# -- Gets a subet of n iterates of the dataset

#' Gets a subet of n iterates of the dataset
#'
#' @param data The analysed dataframe
#' @param n Number of unique articles
#'
#' @return The dataframe with only n unique iterates
#' @export
#'
#' @examples get_subset(salesdata::sales_daily, n = 1)
#'
get_subset <- function(data, n) {

  # check for correct input data
  check_input_data(data)

  used_iterates <- get_unique_iterates(data) %>%
    sample(n, replace = F)

  dplyr::filter(data, iterate %in% used_iterates)

}

# -- Verify the data input has the correct format

#' Function verifies that the dataframe has the correcht column names and
#' datatypes. You need following structure:
#'
#' - date column: datatype date
#' - iterate column: datatype charachter
#' - y column: datatype numeric
#'
#' @param data The checked input data
#'
#' @return Throws an error if the structure is wrong
#' @export
#'
#' @examples  check_input_data(salesdata::sales_monthly)

check_input_data <- function(data) {

  # check for correct names
  check_correct_name(data)

  # check for correct datatypes
  check_correct_datatype(data)
}


# -- internal helper functions ----


# function returns TRUE if the passed column name is missing
check_column_missing <- function(data, columnname) {

  if(columnname %in% colnames(data) == F) {

    # the columnname is missing
    return(TRUE)
  } else {

    # the columname is in the dataset
    return(FALSE)
  }
}

#The function checks if alle the needed columns are in the dataframe.
check_correct_name <- function(data) {

  if(check_column_missing(data, "iterate")) {

    #throw an error if iterate column is missung
    stop("iterate column is missing")
  }

  if(check_column_missing(data, "date")) {

    #throw an error if date column is missing
    stop("date column is missing")
  }

  if(check_column_missing(data, "y")) {

    #throw an error if y column is missing
    stop("y column is missing")
  }

}

# The function checks if all the correct datatypes are there for the needed
# columns.
check_correct_datatype <- function(data) {

  # check date column is datatype date
  if(class(data$date) != "Date") {

    #throw an error if data column is not datatype date
    stop("The date column is no date datatype")
  }

  # check iterate column is character datatype
  if(class(data$iterate) != "character") {
    # throw an errir if iterate column is not datatype charcter
    stop("The iterate column is no character datatype")
  }

  # check y column is numeric.. integer or double or numeric
  if(class(data$y) != "integer" &
     class(data$y) != "double" &
     class(data$y) != "numeric") {
    stop("The y column is no integer or double datatype")
  }
}


