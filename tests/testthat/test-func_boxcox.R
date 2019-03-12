# Tests for boxcox transformations.

context("test-func_boxcox")


# -- Test boxcox transformation and backtransformation is working. ----
test_that("boxcox_inverse", {

  sales_data <- sales_monthly

  # We retrieve the lambdas for the dataset. With these lambdas we can revert
  # the boxcox transformation.
  lambdas <- tf_get_lambdas(sales_data)

  #Apply the boxcox transformation
  sales_data_trans <- tf_boxcox(sales_data)

  # now we remove the transformation
  sales_data_retransformed <- tf_remove_boxcox(sales_data_trans, lambdas) %>%

    # we round the numbers in order to compare for resulting differences due to the
    # backtransformation
    dplyr::mutate(y = round(y,0))

  # number of rows with a difference unequal. This means boxcos transformation
  # and backtransforamtion isn't working properly.

  nrows <- sales_data %>%
    dplyr::left_join(
      sales_data_retransformed,
      by = c("date", "iterate"),
      suffix = c("_orig", "_trans")
    ) %>%
    dplyr::mutate(diff = y_orig - y_trans) %>%
    dplyr::filter(diff != 0) %>%
    nrow()

  expect_equal(nrows, 0)

})

# -- Test retrieving the lambdas is working ----
test_that("boxcox_lambda", {

  sales_data <- sales_monthly

  # get an example lambda value
  lambda <- tf_get_lambdas(sales_data) %>%
    head(1) %>%
    dplyr::pull(lambda) %>%
    round(., 7)

  expect_equal(0.1017398, lambda)
})

# -- Test the boxcox transformation is working ----
test_that("boxcox_trans", {

  sales_data <- sales_monthly

  transed_value <- tf_boxcox(sales_data) %>%
    head(1) %>%
    dplyr::pull(y) %>%
    round(., 5)

  expect_equal(10.35487, transed_value)
})
