% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_forecasts.R
\name{create_forecast}
\alias{create_forecast}
\title{Actually calls the forecast functions and applies sweep to the result.}
\usage{
create_forecast(data, mod, n_pred)
}
\arguments{
\item{mod}{The pretrained model.}

\item{n_pred}{The number of predictions}
}
\value{
The produced forecast. The date column has to be corrected.
}
\description{
Actually calls the forecast functions and applies sweep to the result.
}
\examples{
\dontrun{create_forecasts(sales_data, auto.arima, 6)}

}
