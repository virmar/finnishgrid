% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finnishgrid.R
\name{transmission_capacity_FI_to_RUS}
\alias{transmission_capacity_FI_to_RUS}
\title{Transmission capacity FI-RUS}
\usage{
transmission_capacity_FI_to_RUS(start_time = NA, end_time = NA, user_key = NA)
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
A data frame object that contains the total commercial transmission
capacity of the 400 kV transmission lines from Finland to Russia owned
by Fingrid. The technical capacity on 400 kV lines from Russia to
Finland is 1400 MW or 1000 MW, depending whether the NWPP power plant
that is located in St. Petersburg area is connected to the Finnish or
the Russian power system. Fingrid has reserved 100 MW of transmission
capacity from Russia to Finland to buy reserve power. The technical
maximum capacity from Finland to Russia is 350 MW, of which Fingrid has
reserved 30 MW to buy reserve power.
}
\description{
Check https://data.fingrid.fi/en/dataset/summakapasiteetti-fi-rus
}
\examples{
\dontrun{
library(finnishgrid)
start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
key = "MY_SUPER_SECRET"
df <- transmission_capacity_FI_to_RUS(start_time = start,
    end_time = end,
    user_key = key)
summary(df)
}
}
