% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finnishgrid.R
\name{consumption_imbalance_electricity_price}
\alias{consumption_imbalance_electricity_price}
\title{The price of comsumption imbalance electricity}
\usage{
consumption_imbalance_electricity_price(
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
A data frame object that contains the price of consumption
imbalance power is the price for which Fingrid both purchases imbalance
power from a balance responsible party and sells it to one. In the case
of regulating hour, the regulation price is used. If no regulation has
been made, the Elspot FIN price is used as the purchase and selling
price of consumption imbalance power. Separate consumption imbalance
ended when 1.11.2021 01.00 settlement model was changed to single
imbalance.
}
\description{
Check https://data.fingrid.fi/en/dataset/the-price-of-comsumption-imbalance-electricity
}
\examples{
\dontrun{
library(finnishgrid)
start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
key = "MY_SUPER_SECRET"
df <- consumption_imbalance_electricity_price(start_time = start,
    end_time = end,
    user_key = key)
summary(df)
}
}
