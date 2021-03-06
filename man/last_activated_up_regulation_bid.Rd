% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finnishgrid.R
\name{last_activated_up_regulation_bid}
\alias{last_activated_up_regulation_bid}
\title{Price of last activated up-regulation bid, real-time publication}
\usage{
last_activated_up_regulation_bid(start_time = NA, end_time = NA, user_key = NA)
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
A data frame object that contains The price of the last activated
up-regulation bid. The price is published real-time when Finland is a
separate regulation area.
}
\description{
Check https://data.fingrid.fi/en/dataset/real-time-price-of-up-regulation
}
\examples{
\dontrun{
library(finnishgrid)
start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
key = "MY_SUPER_SECRET"
df <- last_activated_up_regulation_bid(start_time = start,
                                       end_time = end,
                                       user_key = key)
summary(df)
}
}
