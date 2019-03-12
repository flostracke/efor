library(readr)

sales_monthly <- readr::read_csv("data-raw/example_data.csv")
readr::write_rds(sales_monthly, "data/raw_data")
usethis::use_data(sales_monthly)

sales_daily <- readr::read_rds("data-raw/example_data_daily.rds")
readr::write_rds(sales_daily, "data/raw_data_daily")
usethis::use_data(sales_daily)
