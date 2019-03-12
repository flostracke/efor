#Tests for log transformation

context("test-func_log")

# -- Tests for tf_log ----

# -- Test log transformation is working ----
test_that("log works", {

  first_log <-  sales_monthly %>%
    tf_log() %>%
    head(1) %>%
    dplyr::pull(y)

  expect_equal(first_log, 7.072422)
})

# -- Test log from negative value is catched ----
test_that("log negative", {

  test_log_negative <- function() {

    tibble::tibble(
      y = c(-2),
      iterate = "test",
      date = as.Date("2017-01-01", format = "%Y-%m-%d")
    ) %>%
      tf_log()
  }
    expect_error(test_log_negative(), "There are negative values in y!")
})

# -- Tests for tf_remove_log ----
test_that("removing log is working", {

  value <- tibble::tibble(date = as.Date("2018-01-01", format = "%Y-%m-%d"),
                          iterate = "Artikel_A",
                          y = 2) %>%
    tf_log() %>%
    tf_remove_log() %>%
    dplyr::pull(y)

  expect_equal(value, 2.0)

})


