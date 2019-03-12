
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/flostracke/efor.svg?branch=master)](https://travis-ci.org/flostracke/efor)
[![Coverage
status](https://codecov.io/gh/flostracke/efor/branch/master/graph/badge.svg)](https://codecov.io/github/flostracke/efor?branch=master)
[![stability-experimental](https://img.shields.io/badge/stability-experimental-orange.svg)](https://github.com/joethorley/stability-badges#experimental)

# efor

The goal of efor is to make it easier if you have to forecast or apply
preprocessing to multiple timeseries. Furthermore the the package
supports in transforming timeseries data for applying machine learning
or deeplearning to it. (Creation of lagged values, boxcox
transformation)

## Installation

You can install the released version of efor from github with:

``` r
#devtools::install_github("flostracke/efor")
```

## Example

First we load some packages for this example.

``` r

library(salesdata) # devtools::install_github("flostracke/salesdata"). Some example data
library(tidyverse)
library(efor)
library(furrr) # for running the forecasting in parallel
library(forecast) #provides forecast mehotds
library(prophet) # provides forecast mehod
library(tidyquant) # for nicer plots

sales_data <- sales_monthly
```

We have some sales data for four articles. We want to create forecasts
for all these articles. The efor package makes this quite easy, because
it provides functionality to create forecasts for multiple articles with
one line of code. Right now you can use the forecast package or the
prophet package.

``` r

ggplot(sales_data, aes(x = date, y = y)) +
  geom_line() +
  geom_point() +
  facet_wrap(~iterate) +
  ggtitle("The original series") +
  theme_tq() 
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

We split the dataset in a train and test set. All observations from the
year 2016 go to the test set. We want to create forecasts for the next 4
months of the testset and evaluate the performance.

``` r
train_data <- sales_data %>% 
  filter(date < "2016-01-01")

test_data <- sales_data %>% 
  filter(date >= "2016-01-01")
```

Now we can apply the the auto.arima function to the dataset and create
the forecasts.

``` r
forecasts_ar <- tf_grouped_forecasts(
  train_data, # used training dataset
  n_pred = 4, # number of predictions
  func = auto.arima, # used forecasting method
  freq = 12 # Frequency. 12 for monthly data, 1 for daily data
)
```

With the same syntax you can create forecasts utilizing the prophet
package: Please note that there we disable the parallel function for
prophet, because there is some bug right now.

``` r
forecasts_prophet <- tf_grouped_forecasts(
  train_data, # used training dataset
  n_pred = 4, # number of predictions
  func = prophet, # used forecasting method
  freq = 12, # Frequency. 12 for monthly data, 1 for daily data
  parallel = FALSE
)
```

In order to create some plots and evaluate the performance we combine
the forecasts into one dataset.

``` r
forecasts <- bind_rows(forecasts_ar, forecasts_prophet)
```

The package brings also a function which makes ist quite easy to access
the performance (right now thhe mae, rmse and rsquared is calculated)of
multiple methods:

``` r
tf_calc_metrics(forecasts, test_data)
#> # A tibble: 6 x 3
#>   key        metric   value
#>   <chr>      <chr>    <dbl>
#> 1 auto.arima mae    154.   
#> 2 prophet    mae    157.   
#> 3 prophet    rmse   217.   
#> 4 auto.arima rmse   222.   
#> 5 auto.arima rsq      0.847
#> 6 prophet    rsq      0.858
```

Also it is possible to access the performance of each article:

``` r
tf_calc_metrics(forecasts, test_data, detailed = TRUE)
#> # A tibble: 24 x 4
#>    key        iterate   metric value
#>    <chr>      <chr>     <chr>  <dbl>
#>  1 auto.arima Article_B mae     71  
#>  2 prophet    Article_C mae     88.3
#>  3 prophet    Article_B mae     90.0
#>  4 auto.arima Article_C mae    123. 
#>  5 auto.arima Article_A mae    177. 
#>  6 prophet    Article_A mae    212. 
#>  7 prophet    Article_D mae    237. 
#>  8 auto.arima Article_D mae    243. 
#>  9 auto.arima Article_B rmse    86.2
#> 10 prophet    Article_C rmse   102. 
#> # ... with 14 more rows
```

Finally we create a quick graph visualising the results of the
forecasts.

``` r
train_data_plot <- train_data %>% 
  mutate(key = "train")

test_data_plot <- test_data %>% 
  mutate(key = "test")

bind_rows(train_data_plot, test_data_plot) %>% 
  bind_rows(forecasts) %>% 
  ggplot(aes(x = date, y = y, color = key)) +
  geom_point() +
  geom_line() +
  facet_wrap(~iterate) +
  ggtitle("Forecasted values for each article") +
  ylab("Sales amount") +
  theme_tq() 
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

And we can plot the forecasts against the actual forecasts:

``` r
tf_plot_preds_actuals(forecasts, test_data)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

Add the distribtuion of the residuals:

``` r
tf_plot_residuals(forecasts, test_data)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />
