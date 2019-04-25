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

dtl_metric <- tf_calc_metrics(forecasts_prophet, test_data, metrics = c("rmse", "mae", "rsq", "mase", "mape"), detailed = TRUE) %>%
  filter(metric == relevant_metric)

relevant_iterates <-  dtl_metric %>%
  #Make it dynamic to get the best or the worst article  -> if else with head / tail
  head(n_iterates) %>%
  pull(iterate)

#prepare the labels with ordered rmse
labels <- dtl_metric %>%
  filter(metric == relevant_metric) %>%
  dplyr::select(iterate, value) %>%
  dplyr::rename(metric = value) %>%
  dplyr::mutate(metric_order = metric) %>%
  dplyr::mutate(metric = as.character(round(metric, 2))) %>%
  dplyr::mutate(metric = stringr::str_c(" (", metric)) %>%
  dplyr::mutate(metric = stringr::str_c(metric, ")")) %>%
  dplyr::mutate(new_label = stringr::str_c(iterate, metric)) %>%
  #reorder factors for plotting ascending rmse
  dplyr::mutate(new_label = forcats::as_factor(new_label)) %>%
  dplyr::mutate(new_label = forcats::fct_reorder(new_label, metric_order )) %>%
  dplyr::select(-metric, -metric_order)


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
  dplyr::left_join(labels, by = c("iterate")) %>%
  ggplot(aes(x = date, y = y, color = key)) +
  geom_point() +
  geom_line() +
  geom_ribbon(data = forecasts_plot, aes(ymin = y_lo_95, ymax = y_hi_95), fill = "blue", alpha = 0.2, colour = NA) +
  ggplot2::facet_wrap(~new_label) +
  labs(
    title = "The best n Forecasts from the Prophet model",
    subtitle = "regarding the rmse"
  )
  theme_minimal()
