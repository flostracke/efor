# Tests for all the utils functions.

context("test-func_utils")

# -- test the check input data column

# the main function
test_that("check the main test function", {
  exdata <- sales_monthly %>%
    dplyr::mutate(y = as.character(y))

  expect_error(check_input_data(exdata))
})

# test get_unique_iterates ----
test_that("check get unique iterates", {

  input_iterates <- tibble::tibble(iterate = c("A", "B", "C", "C"))
  expected_iterates <- c("A", "B", "C")

  expect_equal(expected_iterates, get_unique_iterates(input_iterates))
})

# -- tests for correct column names ----

# date column
test_that("check date column exists", {
  exdata <- sales_monthly %>%
    dplyr::rename(column1 = date)

  expect_error(check_correct_name(exdata))
})

# iterate column
test_that("check iterate column exists", {
  exdata <- sales_monthly %>%
    dplyr::rename(column1 = iterate)

  expect_error(check_correct_name(exdata))
})

# y column
test_that("check y column exists", {
  exdata <- sales_monthly %>%
    dplyr::rename(column1 = y)

  expect_error(check_correct_name(exdata))
})

# -- tests for correct datatypes ----


test_that("check date column is correct datatype", {
  exdata <- sales_monthly %>%
    dplyr::mutate(date = as.character(date))

  # since we removed the check for a date datatype we expect no error message
  expect_error(check_correct_datatype(exdata), NA)
})

#iterate column
test_that("check iterate column is correct datatype", {

  exdata <- sales_monthly %>%
    dplyr::mutate(iterate = n()) %>%
    dplyr::mutate(iterate = as.integer(iterate))

  expect_error(check_correct_datatype(exdata))
})

# check y column
test_that("check y column is correct datatype", {
  exdata <- sales_monthly %>%
    dplyr::mutate(y = as.character(y))

  expect_error(check_correct_datatype(exdata))
})

# test get_unique_iterates
test_that("get unique iterates", {

  exdata <- sales_monthly
  expected_res <- c("Article_A", "Article_B", "Article_C", "Article_D")
  expect_equal(get_unique_iterates(exdata), expected_res)
})

test_that("get subset", {

  testdata <- sales_monthly
  set.seed(10)
  expected_res <- "Article_C"

  res <- get_subset(testdata, 1) %>%
    get_unique_iterates()

  expect_equal(expected_res, res)

})
