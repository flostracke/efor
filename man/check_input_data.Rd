% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_utils.R
\name{check_input_data}
\alias{check_input_data}
\title{Function verifies that the dataframe has the correcht column names and
datatypes. You need following structure:}
\usage{
check_input_data(data)
}
\arguments{
\item{data}{The checked input data}
}
\value{
Throws an error if the structure is wrong
}
\description{
- date column: datatype date
- iterate column: datatype charachter
- y column: datatype numeric
}
\examples{
 check_input_data(sales_monthly)
}
