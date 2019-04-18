# Tests for forecasting

context("test-func_forecasts")

test_that("Forecasting produces correct number of results for each article", {

  library(furrr)
  library(dplyr)
  library(tsibble)

  #get the number of expected forecasts in the result
  forecast_horizon <- 3L

  sales_monthly_ts <- sales_monthly %>%
    mutate(date = yearmonth(date)) %>%
    as_tsibble(
      key = id(iterate),
      index = date
    )

  #get the number of expected articles
  nr_articles <- sales_monthly_ts %>%
    get_unique_iterates() %>%
    length()

  #List with the number of to be produced forecasts and the number of articles
  expected_result <- list(forecast_horizon, nr_articles)

  #produce the forecast
  forecasts <- tf_grouped_forecasts(
    sales_monthly_ts,
    n_pred = forecast_horizon,
    forecast::auto.arima,
    freq = 12,
    parallel = FALSE
  )

  #get the number of articles in the forecast result
  nr_articles_forecast <- forecasts %>%
    get_unique_iterates() %>%
    length()
  #get the number of the produced forecast
  nr_produced_forecasts <- forecasts %>%
    as_tibble() %>%
    count(iterate) %>%
    pull(n) %>%
    mean()

  #build the result of the forecast in order to compare it against the expected output
  actual_result <- list(nr_produced_forecasts, nr_articles_forecast)

  expect_equal(expected_result, actual_result)

})


