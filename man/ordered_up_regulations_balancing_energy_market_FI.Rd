% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finnishgrid.R
\name{ordered_up_regulations_balancing_energy_market_FI}
\alias{ordered_up_regulations_balancing_energy_market_FI}
\title{Ordered up-regulations from Balancing energy market in Finland}
\usage{
ordered_up_regulations_balancing_energy_market_FI(
  start_time = NA,
  end_time = NA,
  user_key = NA
)
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
A data frame object that contains ordered up-regulations from
Balancing energy market in Finland. The volume of ordered up-regulations
from Balancing energy market in Finland is published hourly with two
hours delay, eg. information from hour 06-07 is published at 9 o'clock.
}
\description{
Check https://data.fingrid.fi/en/dataset/ordered-up-regulations-from-balancing-energy-market-in-finland
}
\examples{
\dontrun{
library(finnishgrid)
start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
key = "MY_SUPER_SECRET"
df <- ordered_up_regulations_balancing_energy_market_FI(start_time = start,
    end_time = end,
    user_key = key)
summary(df)
}
}
