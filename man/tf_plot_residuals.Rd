% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_plots.R
\name{tf_plot_residuals}
\alias{tf_plot_residuals}
\title{Plots the distribtuion of the residuals of each method}
\usage{
tf_plot_residuals(forecasts, testset)
}
\arguments{
\item{forecasts}{The dataset with the forecasted values}

\item{testset}{The testset with the acutal values}
}
\description{
Plots the distribtuion of the residuals of each method
}
\examples{
tf_plot_residuals(sales_forecast, sales_test)
}
