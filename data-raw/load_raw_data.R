library(tidyverse)
library(efor)
library(furrr)
library(prophet)

sales_monthly <- readr::read_csv("data-raw/example_data.csv")
readr::write_rds(sales_monthly, "data/raw_data")
usethis::use_data(sales_monthly)

sales_daily <- readr::read_rds("data-raw/example_data_daily.rds")
readr::write_rds(sales_daily, "data/raw_data_daily")
usethis::use_data(sales_daily)

# split into train and test set

sales_train <- sales_monthly %>%
  filter(date < "2016-01-01")
usethis::use_data(sales_train)

sales_test <- sales_monthly %>%
  filter(date >= "2016-01-01")
usethis::use_data(sales_test)

#create auto.arima forecast
forecasts_ar <- tf_grouped_forecasts(
  sales_train, # used training dataset
  n_pred = 4, # number of predictions
  func = forecast::auto.arima, # used forecasting method
  freq = 12 # Frequency. 12 for monthly data, 1 for daily data
)

#create prophet forecast
forecasts_prophet <- tf_grouped_forecasts(
  sales_train, # used training dataset
  n_pred = 4, # number of predictions
  func = prophet, # used forecasting method
  freq = 12, # Frequency. 12 for monthly data, 1 for daily data
  parallel = FALSE
)

sales_forecast <- bind_rows(forecasts_ar, forecasts_prophet)
usethis::use_data(sales_forecast)
