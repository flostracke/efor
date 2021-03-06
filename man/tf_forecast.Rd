% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_forecasts.R
\name{tf_forecast}
\alias{tf_forecast}
\title{Creates forecast for a dateframe.}
\usage{
tf_forecast(data, n_pred, func, ...)
}
\arguments{
\item{data}{The dataframe containing the timeseries.}

\item{n_pred}{The forecast horizon.}

\item{func}{The used forecast method.}

\item{...}{More arguments specific to the used forecasting method.}
}
\value{
The forecasted values for the series
}
\description{
The Dataframe has to have the columny date and y. Following methods are
supported:
}
\details{
\itemize{
    \item {forecasts package: All methods from the forecast packages
    (\url{https://CRAN.R-project.org/package=forecast}) should work}
    \item {prophet: (\url{https://CRAN.R-project.org/package=prophet})}
    \item {forecastHybrid: (\url{https://CRAN.R-project.org/package=forecastHybrid })}
    \item {smooth package: (\url{https://CRAN.R-project.org/package=smooth})}
}
}
\examples{
\dontrun{tf_forecast(
           train_data,        # used training dataset
           n_pred = 6,        # number of predictions
           func = auto.arima, # used forecasting method
           parallel = TRUE    # for runiing in parallel
         )}

}
