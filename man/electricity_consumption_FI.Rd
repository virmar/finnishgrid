% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finnishgrid.R
\name{electricity_consumption_FI}
\alias{electricity_consumption_FI}
\title{Electricity consumption in Finland}
\usage{
electricity_consumption_FI(start_time = NA, end_time = NA, user_key = NA)
}
\arguments{
\item{start_time}{Start time in UTC with offset. Character array in ISO8601,
YYYY-MM-ddTHH:mm:ssZ}

\item{end_time}{End time in UTC with offset. Character array in ISO8601,
YYYY-MM-ddTHH:mm:ssZ}

\item{user_key}{Character array holding API-key.
Free from https://data.fingrid.fi/open-data-forms/registration/}
}
\value{
A data frame object that contains electricity consumption in
Finland is based on Fingrid's production measurements. Minor part of
production which is not measured is estimated. The consumption is
calculated as follows: Consumption = Production + Import - Export.
Updated hourly.
}
\description{
Check https://data.fingrid.fi/en/dataset/electricity-consumption-in-finland
}
\examples{
\dontrun{
library(finnishgrid)
start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
key = "MY_SUPER_SECRET"
df <- electricity_consumption_FI(
    start_time = start,
    end_time = end,
    user_key = key)
summary(df)
}
}
