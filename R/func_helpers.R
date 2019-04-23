# -- Different helper functions

# -- external functions ----

#' Function creates a dataframe with the forecasts. The forecasts will be
#' combined with the testset.
#'
#' @param v_forecasts The created forecasts. You can pass a numeric vector or
#'                    a mlr Prediction object.
#' @param df_test The Testset which should be enriched with the forecasts.
#' @param name The name of the forecasting method.
#'
#' @return The Testset Dataframe, combined with the created forecasts.
#' @export
#'
#' @examples
#'
tf_build_forecast <- function(v_forecasts, df_test, name) {

  #Case numeric vector
  if (class(v_forecasts)[1] == "numeric") {

    create_final_forecast_df(v_forecasts, df_test, name)
  }

  #Case mlr prediction object
  else {

    v_mlr_pred <- v_forecasts$data %>%
      tibble::as_tibble() %>%
      dplyr::select(-truth, y = response) %>%
      pull()

    create_final_forecast_df(v_mlr_pred, df_test, name)
  }
}

# -- internal functions ----

# Helper Funktion, die ein Forecast DF erzeugt.
create_final_forecast_df <- function(v_forecasts, df_test, name) {
  df_test %>%
    dplyr::select(-y) %>%
    dplyr::mutate(
      y = v_forecasts,
      key = name
    )
}



# Functions return the string name of a passed function
find_original_name <- function(fun) {

  objects <- ls(envir = environment(fun))
  for (i in objects) {
    if (identical(fun, get(i, envir = environment(fun)))) {
      return(i)
    }
  }
}


# Helper function for getting used os
get_os <- function() {

  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  stringr::str_flatten(tolower(os))
}

#Helper function for creating a plan for the future package
create_plan <- function(parallel = T) {

  if (parallel) {
    # If Windows OS use multiprocess, otherwise multicore
    if (get_os() == "windows") {
      future::plan(multiprocess)
    } else {
      future::plan(multicore)
    }

  } else {
    future::plan(sequential)
  }
}
