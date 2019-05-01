library(tidyverse)
library(tsibble) # for creating nicer representation of monthly data
#library(efor)
library(furrr) # for running the forecasting in parallel
library(forecast) #provides forecast mehotds
library(prophet)

library(rsample)


sales_data <- sales_monthly %>%
  mutate(date = yearmonth(date))

splits <- sales_data %>%
filter(iterate == "Article_A") %>%
  rsample::rolling_origin(
    initial = 24,
    assess = 6,
    cumulative = TRUE,
    skip = 5
  )

analysis(splits$splits[[2]])

cv_forecasts <- splits %>%
  split(.$id) %>%
  purrr::map_df(tf_grouped_forecasts_cv, func = ets, n_pred = 6)
# https://tidymodels.github.io/rsample/articles/Applications/Time_Series.html
cv_forecasts %>%
  ggplot(aes(x = date, y = y )) +
  geom_line() +
  facet_wrap(~split_id)
