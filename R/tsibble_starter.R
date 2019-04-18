library(tidyverse)
library(tsibble)


data <- salesdata::sales_monthly %>%
  mutate(date = yearmonth(date))

#specifiy numer of batch forecasts
n_pred <- 3L

#create a tibble object
ts_data <- as_tsibble(
    data,
    key = id(iterate),
    index = date,
    validate = TRUE
  ) %>%
  filter(iterate == "Article_A")


func <- forecast::auto.arima

#replace with efor::find_original_name(func)
name <- "auto.arima"


#create future dates for forecasting
future_data <- ts_data %>%
  append_row(n = n_pred) %>%
  tail(n_pred) %>%
  pull(date)

# build the actual model
ar_mod1 <- ts_data %>%
  as.ts() %>%
  forecast::auto.arima()

#create forecasts and clean them
forecast::forecast(ar_mod, n_pred) %>%
  sweep::sw_sweep(fitted = T, rename_index = "date") %>%
  filter(key == "forecast") %>%
  mutate(date = future_data) %>%
  head(n_pred)
