tf_grouped_forecasts_cv <- function(data, n_pred, func, parallel = TRUE, ...) {

  #if it is a rsample split object get the train part of the split
  data <- rsample::analysis(data)

  #get a vector with the original date format. workaounrd for bug in
  #bind_rows, which looses the original date column data type
  #orig_future_dates <- build_final_date_vector(data, n_pred)
  orig_future_dates <- build_final_date_vector(data, n_pred)

  #create plan for multiprocessing
  create_plan()

  if(parallel == FALSE) {

    # Prophet funktioniert nicht mit future map.. nur purr map nutzen!
    forecasts <- data %>%
      split(.$iterate) %>%
      purrr::map(tf_forecast, n_pred, func, ...) %>%
      dplyr::bind_rows()

  } else {

    #TODO Add Test for parallel path
    forecasts <- data %>%
      split(.$iterate) %>%
      furrr::future_map(tf_forecast, n_pred, func, ...) %>%
      dplyr::bind_rows()
  }

  #add the original date colmntye
  forecasts <- dplyr::mutate(forecasts, date = orig_future_dates)

  return(forecasts)

}
