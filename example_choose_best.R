library(tidyverse)
library(efor)
library(furrr) # for running the forecasting in parallel
library(forecast) #provides forecast mehotds
library(prophet) # provides forecast mehod
library(tidyquant) # for nicer plots

sales_data <- sales_monthly

# Build Train, Valid, and Testset ----
train_data <- sales_data %>%
  filter(date < "2016-01-01") %>%
  mutate(key = "train")

valid_data <- sales_data %>%
  filter(date >= "2016-01-01", date <= "2016-03-01") %>%
  mutate(key = "valid")

test_data <- sales_data %>%
  filter(date > "2016-03-01") %>%
  mutate(key = "test")

all_data <- train_data %>%
  bind_rows(valid_data) %>%
  bind_rows(test_data)
 # Plot the split ----
ggplot(all_data, aes(x = date, y = y, color = key)) +
  geom_line() +
  geom_point() +
  facet_wrap(~iterate) +
  ggtitle("The original series") +
  theme_tq()

# train models against valid set

forecasts_ar <- tf_grouped_forecasts(
  train_data, # used training dataset
  n_pred = 3, # number of predictions
  func = auto.arima, # used forecasting method
  freq = 12 # Frequency. 12 for monthly data, 1 for daily data
)

forecasts_prophet <- tf_grouped_forecasts(
  train_data, # used training dataset
  n_pred = 3, # number of predictions
  func = prophet, # used forecasting method
  freq = 12, # Frequency. 12 for monthly data, 1 for daily data
  parallel = FALSE
)

forecasts_tbats <- tf_grouped_forecasts(
  train_data, # used training dataset
  n_pred = 3, # number of predictions
  func = tbats, # used forecasting method
  freq = 12, # Frequency. 12 for monthly data, 1 for daily data
  parallel = FALSE
)

forecasts_ets <- tf_grouped_forecasts(
  train_data, # used training dataset
  n_pred = 3, # number of predictions
  func = ets, # used forecasting method
  freq = 12, # Frequency. 12 for monthly data, 1 for daily data
  parallel = FALSE
)

forecasts_valid <- bind_rows(forecasts_ar, forecasts_prophet) %>%
  bind_rows(forecasts_tbats) %>%
  bind_rows(forecasts_ets)

best_method <- tf_get_best_method(forecasts_valid, select(valid_data,-key))

# Erstelle nun Forecasts mit der jeweils besten methode:

train_data <- bind_rows(train_data, valid_data)

forecasts_ar <- tf_grouped_forecasts(
  train_data, # used training dataset
  n_pred = 3, # number of predictions
  func = auto.arima, # used forecasting method
  freq = 12 # Frequency. 12 for monthly data, 1 for daily data
)

forecasts_prophet <- tf_grouped_forecasts(
  train_data, # used training dataset
  n_pred = 3, # number of predictions
  func = prophet, # used forecasting method
  freq = 12, # Frequency. 12 for monthly data, 1 for daily data
  parallel = FALSE
)

forecasts_tbats <- tf_grouped_forecasts(
  train_data, # used training dataset
  n_pred = 3, # number of predictions
  func = tbats, # used forecasting method
  freq = 12, # Frequency. 12 for monthly data, 1 for daily data
  parallel = FALSE
)

forecasts_ets <- tf_grouped_forecasts(
  train_data, # used training dataset
  n_pred = 3, # number of predictions
  func = ets, # used forecasting method
  freq = 12, # Frequency. 12 for monthly data, 1 for daily data
  parallel = FALSE
)

forecasts_test <- bind_rows(forecasts_ar, forecasts_prophet) %>%
  bind_rows(forecasts_tbats) %>%
  bind_rows(forecasts_ets) %>%
  mutate(key_orig = key)

forecasts_best <- best_method %>%
  inner_join(forecasts_test, by = c("iterate", "key")) %>%
  mutate(key = "best_method_val")

forecasts_test <- forecasts_test %>%
  bind_rows(forecasts_best)

tf_calc_metrics(forecasts_test, select(test_data, -key))

tf_plot_preds_actuals(forecasts_test, select(test_data, -key))
