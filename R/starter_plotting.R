library(tidyverse)
library(efor)
library(furrr) # for running the forecasting in parallel
library(forecast) #provides forecast mehotds
library(prophet) # provides forecast mehod
library(tsibble)


sales_data <- sales_monthly %>%
  mutate(date = yearmonth(date))

train_data <- sales_data %>%
  filter(date < "2016-01-01")

test_data <- sales_data %>%
  filter(date >= "2016-01-01")

forecasts_ar <- tf_grouped_forecasts(
  train_data,        # used training dataset
  n_pred = 6,        # number of predictions
  func = auto.arima, # used forecasting method
  parallel = TRUE    # for runiing in parallel
)

forecasts_prophet <- tf_grouped_forecasts(
  train_data,      # used training dataset
  n_pred = 6,      # number of predictions
  func = prophet,  # used forecasting method
  parallel = FALSE #disabling parallel for prohet
)

forecasts <- bind_rows(forecasts_ar, forecasts_prophet) %>%
  mutate(date = yearmonth(date))

#TODO Check for only one forecast method!

#get the best n articles based on the rmse
#TODO make metric dynamic!
relevant_metric <- "rmse"
n_iterates <- 2L

relevant_iterates <- tf_calc_metrics(forecasts_prophet, test_data, metrics = c("rmse", "mae", "rsq", "mase", "mape"), detailed = TRUE) %>%
  filter(metric == relevant_metric) %>%
  #Make it dynamic to get the best or the worst article  -> if else with head / tail
  head(n_iterates) %>%
  pull(iterate)


train_data_plot <- train_data %>%
  filter(iterate %in% relevant_iterates) %>%
  mutate(key = "train")

test_data_plot <- test_data %>%
  filter(iterate %in% relevant_iterates) %>%
  mutate(key = "test")

forecasts_plot <- forecasts_prophet %>%
  filter(iterate %in% relevant_iterates)

#save original date worker because of bug in bind_rows loosing the date format
orig_dates <- c(train_data_plot$date, test_data_plot$date, forecasts_plot$date)

bind_rows(train_data_plot, test_data_plot) %>%
  bind_rows(forecasts_plot) %>%
  mutate(date = orig_dates) %>%
  ggplot(aes(x = date, y = y, color = key)) +
  geom_point() +
  geom_line() +
  geom_ribbon(data = forecasts_plot, aes(ymin = y_lo_95, ymax = y_hi_95), fill = "blue", alpha = 0.2, colour = NA) +
  facet_wrap(~iterate) +
  ggtitle("Forecasted values for n best iterates") +
  theme_minimal()
