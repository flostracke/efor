# Tests for adding lagged variables

context("test-func_lag")

test_that("creating lagged variables works", {

  testdata <- salesdata::sales_monthly
  firstvalue <- testdata[1, "y"]

  lagged_data <- tidypreprocess::tf_create_lags(testdata, lag_var = y, 1:4)
  lagged_value <- lagged_data[2, "y_lag_1"]

  expect_equal(firstvalue$y, lagged_value$y_lag_1)
})

test_that("error when number of lags not integer", {

  expect_error(check_n_lags(1.1:5.0))
})

