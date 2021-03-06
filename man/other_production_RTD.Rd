% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finnishgrid.R
\name{other_production_RTD}
\alias{other_production_RTD}
\title{Other production inc. estimated small-scale production and reserve power plants - real time data}
\usage{
other_production_RTD(start_time = NA, end_time = NA, user_key = NA)
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
A data frame object that contains reserve power plants electrical
production is based on the real-time measurements in Fingrid's operation
control system. Estimated small-scale production is added, of which
there are no measurements available. The data is updated every 3 minutes.
}
\description{
Check https://data.fingrid.fi/en/dataset/gas-turbine-and-estimated-small-scale-production-real-time-data
}
\examples{
\dontrun{
library(finnishgrid)
start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
key = "MY_SUPER_SECRET"
df <- other_production_RTD(
    start_time = start,
    end_time = end,
    user_key = key)
summary(df)
}
}
