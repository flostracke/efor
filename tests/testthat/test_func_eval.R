# Tests for adding lagged variables

context("test-func_evaluation")

test_that("Test yardstick::metrics caluclation", {

forecasts <- tibble::tibble(
  date = c("2018-01-01", "2018-01-02", "2018-01-03"),
  iterate = "Article_A",
  y = c(5, 10, 15),
  key = "manual"
)

testset <- tibble::tibble(
  date = c("2018-01-01", "2018-01-02", "2018-01-03"),
  iterate = "Article_A",
  y = c(7, 12, 15)
)

expected_errors <- c(1.3333333, 1.6329932,0.9795918, 0.3333333, 15.0793651)

calculated_errors <- tf_calc_metrics(forecasts, testset, detailed = TRUE) %>%
  dplyr::pull(value)

expect_equal(expected_errors, calculated_errors)

})


