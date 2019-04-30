library(tidyverse)
library(tsibble) # for creating nicer representation of monthly data
#library(efor)
library(furrr) # for running the forecasting in parallel
library(forecast) #provides forecast mehotds

library(rsample)


sales_data <- sales_monthly %>%
  mutate(date = yearmonth(date))

splits <- sales_data %>%
filter(iterate == "Article_A") %>%
  rsample::rolling_origin(
    initial = 24,
    assess = 12,
    cumulative = TRUE,
    skip = 2
  )

analysis(splits$splits[[2]])

purrr::map_df(splits$splits, tf_grouped_forecasts_cv, func = auto.arima, n_pred = 12)


