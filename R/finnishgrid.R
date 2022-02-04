# ----- MAIN API CALL FUNCTION (GET_DATA) -----
#

#' @title Main logic forming the API call.
#'
#' @description Main logic forming the API call. API key
#'     can be provided as function parameter or environment
#'     variable (in .Renviron as FINGRID_OPENDATA_API_KEY).
#'     Function parameter has precedence in case both are provided.
#'     For API spec see https://data.fingrid.fi/en/pages/api.
#'
#' @param api_number Integer related to the Fingrid Open Data API
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#'
#' @return A data frame object that contains wanted open data.
#'
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- get_data(api_number = 124,  # electricity demand for Finland
#'                start_time = start,
#'                end_time = end,
#'                user_key = key)
#' summary(df)
#' }
#' @importFrom httr GET status_code http_type add_headers timeout
#' @importFrom jsonlite fromJSON
#' @export
get_data <- function(api_number = NA,
                     start_time = NA,
                     end_time = NA,
                     user_key = NA) {
  if (is.na(api_number)) {
    stop("API number was not given")
  }
  if (is.na(start_time)) {
    stop("Start time was not given. Format YYYY-MM-ddTHH:mm:ssZ")
  }
  if (is.na(end_time)) {
    stop("End time was not given. Format YYYY-MM-ddTHH:mm:ssZ")
  }
  api_key <- Sys.getenv('FINGRID_OPENDATA_API_KEY')
  if (is.na(user_key) && api_key == "") {
    stop("The API user key was not given")
  }
  # user provided key always overrides the environment variable
  if (!is.na(user_key))
  {
    api_key <- user_key
  }

  # api call using httr
  raw_df <-
    httr::GET(
      url = paste0("http://api.fingrid.fi/v1/variable/",
                   api_number,
                   "/events/json"),
      query = list(start_time = start_time,
                   end_time = end_time),
      httr::add_headers(`x-api-key` = api_key,
                  version = "Fingrid-R-CRAN-client 0.1"),
      httr::timeout(30)  # 30 secs
    )

  # rough checks
  if (httr::http_type(raw_df) != "application/json") {
    stop("API did not return json")
  }

  if (httr::status_code(raw_df) != 200) {
    stop(paste0(
      "API did not succeed (HTTP error status: ",
      httr::status_code(raw_df),
      ")"
    ))
  }

  df <- data.frame(jsonlite::fromJSON(httr::content(raw_df, "text")))

  # If data is empty (no data), return NULL
  if (nrow(df) == 0) {
    message(
      paste0(
        "No datapoints found for API number: ",
        api_number,
        "\nTimespan: ",
        start_time,
        "-",
        end_time,
        "\nReturning NULL for it"
      )
    )
    return(NULL)
  } else {
    df$start_time <- strptime(df$start_time,
                              format = "%Y-%m-%dT%H:%M:%S%z")  # ISO8601
    df$end_time <- strptime(df$end_time,
                            format = "%Y-%m-%dT%H:%M:%S%z")  # ISO8601
    df$id <- api_number
    # reorder: 1.date (start), 2.date (end), 3.value, 4.id
    df <- df[, c("start_time", "end_time", "value", "id")]
    return(unique(df))  # Duplicates are deleted as these cause problems
  }

}


# ----- API HELPER FUNCTIONS -----
#
# Convenient named functions for API calls.

#' @title Procured automatic frequency restoration reserve capacity, up
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/procured-automatic-frequency-restoration-reserve-capacity-up
#' @return A data frame object that contains Procured automatic Frequency
#'     Restoration Reserve (aFRR) capacity, up in MW.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- procured_aFRR_capacity_up(start_time = start,
#'                                 end_time = end,
#'                                 user_key = key)
#' summary(df)
#' }
#' @export
procured_aFRR_capacity_up <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 1,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Procured automatic frequency restoration reserve Capacity, down
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/procured-automatic-frequency-restoration-reserve-capacity-down
#' @return A data frame object that contains Procured automatic Frequency
#'     Restoration Reserve (aFRR / FRR-A) capacity, down in MW.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- procured_aFRR_capacity_down(start_time = start,
#'                                   end_time = end,
#'                                   user_key = key)
#' summary(df)
#' }
#' @export
procured_aFRR_capacity_down <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 2,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Price of last activated up-regulation bid, real-time publication
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/real-time-price-of-up-regulation
#' @return A data frame object that contains The price of the last activated
#'     up-regulation bid. The price is published real-time when Finland is a
#'     separate regulation area.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- last_activated_up_regulation_bid(start_time = start,
#'                                        end_time = end,
#'                                        user_key = key)
#' summary(df)
#' }
#' @export
last_activated_up_regulation_bid <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 22,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Day-ahead transmission capacity SE1-FI
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/intra-day-transmission-capacity-se1-fi
#' @return A data frame object that contains Day-ahead transmission capacity
#'     from North-Sweden (SE1) to Finland (FI). Transmission capacity is given
#'     hourly for every hour of the next day.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- day_ahead_transmission_capacity_SE1_to_FI(start_time = start,
#'                                                 end_time = end,
#'                                                 user_key = key)
#' summary(df)
#' }
#' @export
day_ahead_transmission_capacity_SE1_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 24,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Day-ahead transmission capacity SE3-FI
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/vuorokausimarkkinoille-annettu-siirtokapasiteetti-se3-fi
#' @return A data frame object that contains Day-ahead transmission capacity
#'     from Central-Sweden (SE3) to Finland (FI). Transmission capacity is
#'     given hourly for every hour of the next day.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- day_ahead_transmission_capacity_SE3_to_FI(start_time = start,
#'                                                 end_time = end,
#'                                                 user_key = key)
#' summary(df)
#' }
#' @export
day_ahead_transmission_capacity_SE3_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 25,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Day-ahead transmission capacity FI-SE1
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/vuorokausimarkkinoille-annettu-siirtokapasiteetti-fi-se1
#' @return A data frame object that contains Day-ahead transmission capacity
#'     from Finland (FI) to North-Sweden (SE1). Transmission capacity is given
#'     hourly for every hour of the next day.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- day_ahead_transmission_capacity_FI_to_SE1(start_time = start,
#'                                                 end_time = end,
#'                                                 user_key = key)
#' summary(df)
#' }
#' @export
day_ahead_transmission_capacity_FI_to_SE1 <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 26,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Day-ahead transmission capacity FI-SE3
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/vuorokausimarkkinoille-annettu-siirtokapasiteetti-fi-se3
#' @return A data frame object that contains Day-ahead transmission capacity
#'     from Finland (FI) to Central-Sweden (SE3). Transmission capacity is
#'     given hourly for every hour of the next day.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- day_ahead_transmission_capacity_FI_to_SE3(start_time = start,
#'                                                 end_time = end,
#'                                                 user_key = key)
#' summary(df)
#' }
#' @export
day_ahead_transmission_capacity_FI_to_SE3 <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 27,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Planned weekly capacity from north to south defined by Fingrid
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/planned-weekly-capacity-from-north-to-south-defined-by-fingrid
#' @return A data frame object that contains planned weekly capacity on
#'     North-South cut in Finland (cut P1) from North to South. Planned
#'     outages are included in the weekly capacity, information is not
#'     updated after disturbances.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- weekly_planned_capacity_P1_north_to_south(start_time = start,
#'                                                 end_time = end,
#'                                                 user_key = key)
#' summary(df)
#' }
#' @export
weekly_planned_capacity_P1_north_to_south <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 28,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Planned weekly capacity from south to north defined by Fingrid
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/planned-weekly-capacity-from-south-to-north-defined-by-fingrid
#' @return A data frame object that contains planned weekly capacity on
#'     North-South cut in Finland (cut P1) from South to North. Planned outages
#'     are included in the weekly capacity, information is not updated after
#'     disturbances.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- weekly_planned_capacity_P1_south_to_north(start_time = start,
#'                                                 end_time = end,
#'                                                 user_key = key)
#' summary(df)
#' }
#' @export
weekly_planned_capacity_P1_south_to_north <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 29,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Measured electricity energy in Finland from north to south
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/measured-electricity-energy-in-finland-from-north-to-south
#' @return A data frame object that contains measured electricity flow in
#'     North-South cut in Finland (cut P1). In the graph flow from North
#'     to South is positive.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- measured_electricity_P1_north_to_south(start_time = start,
#'                                              end_time = end,
#'                                              user_key = key)
#' summary(df)
#' }
#' @export
measured_electricity_P1_north_to_south <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 30,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Commercial electricity flow between Finland and Northern Sweden (FI-SE1)
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/commercial-flow-fi-se1
#' @return A data frame object that contains commercial transmission of
#'     electricity (dayahead market and intraday market) between Finland (FI)
#'     and Northern Sweden (SE1). Positive sign is export from Finland to Sweden.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- commercial_electricity_flow_FI_to_SE1(start_time = start,
#'                                             end_time = end,
#'                                             user_key = key)
#' summary(df)
#' }
#' @export
commercial_electricity_flow_FI_to_SE1 <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 31,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Commercial electricity flow between Finland and Mid Sweden (FI-SE3)
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/commercial-flow-fi-se3
#' @return A data frame object that contains commercial electricity flow
#'     (dayahead market and intraday market) between Finland (FI) and Central
#'     Sweden (SE3). Positive sign is export from Finland to Sweden.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- commercial_electricity_flow_FI_to_SE3(start_time = start,
#'                                             end_time = end,
#'                                             user_key = key)
#' summary(df)
#' }
#' @export
commercial_electricity_flow_FI_to_SE3 <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 32,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Ordered down-regulations from Balancing energy market in Finland
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/ordered-down-regulations-from-balancing-market-in-finland
#' @return A data frame object that contains ordered down-regulations from
#'     Balancing energy market in Finland. The volume of ordered
#'     down-regulations from Balancing energy market in Finland is published
#'     hourly with two hours delay, eg. information from hour 06-07 is
#'     published at 9 o'clock.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- ordered_down_regulations_balancing_energy_market_FI(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
ordered_down_regulations_balancing_energy_market_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 33,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Ordered up-regulations from Balancing energy market in Finland
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/ordered-up-regulations-from-balancing-energy-market-in-finland
#' @return A data frame object that contains ordered up-regulations from
#'     Balancing energy market in Finland. The volume of ordered up-regulations
#'     from Balancing energy market in Finland is published hourly with two
#'     hours delay, eg. information from hour 06-07 is published at 9 o'clock.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- ordered_up_regulations_balancing_energy_market_FI(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
ordered_up_regulations_balancing_energy_market_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 34,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Transmission capacity for intraday market from Northern Sweden to Finland (SE1-FI)
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/transmission-capacity-to-be-given-to-intraday-market-se1-fi
#' @return A data frame object that contains transmission capacity for
#'     intraday market from Northern Sweden to Finland (SE1-FI). For intraday
#'     market capacity is given as free capacity after dayahead market.
#'     Capacity is published once a day and not updated.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- transmission_capacity_intraday_market_SE1_to_FI(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
transmission_capacity_intraday_market_SE1_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 38,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Transmission capacity for intraday market from Mid Sweden and Finland (SE3-FI)
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/transmission-capacity-to-be-given-to-intraday-market-se3-fi
#' @return A data frame object that contains transmission capacity for
#'     intraday market from Mid Sweden to Finland (SE3-FI). Capacity for
#'     intraday market is given as free capacity after dayahead market.
#'     Capacity is published once a day and not updated.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- transmission_capacity_intraday_market_SE3_to_FI(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
transmission_capacity_intraday_market_SE3_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 39,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Planned transmission capacity FI-RUS
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/planned-transmission-capacity-fi-rus
#' @return A data frame object that contains planned transmission capacity
#'     from Finland to Russia. Transmission capacity is given hourly for
#'     every next week hour.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- planned_transmission_capacity_FI_to_RUS(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
planned_transmission_capacity_FI_to_RUS <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 41,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Transmission capacity for intraday market from Finland to Northern Sweden (FI - SE1)
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/transmission-capacity-to-be-given-to-intraday-market-fi-se1
#' @return A data frame object that contains transmission capacity for
#'     intraday market from Finland to Northern Sweden (FI - SE1). For
#'     intraday market capacity is given as free capacity after dayahead
#'     market. Capacity is published once a day and not updated.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- transmission_capacity_intraday_market_FI_to_SE1(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
transmission_capacity_intraday_market_FI_to_SE1 <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 44,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Transmission capacity for intraday market from Finland to Mid Sweden (FI - SE3)
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/transmission-capacity-to-be-given-to-intraday-market-fi-se3
#' @return A data frame object that contains transmission capacity for
#'     intraday market from Finland to Mid Sweden (FI - SE3). For intraday
#'     market capacity is given as free capacity after dayahead market.
#'     Capacity is published once a day and not updated.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- transmission_capacity_intraday_market_FI_to_SE3(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
transmission_capacity_intraday_market_FI_to_SE3 <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 45,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Congestion income between Finland and Estonia
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/congestion-income-between-finland-and-estonia
#' @return A data frame object that contains congestion income between
#'     Finland (FI) and Estonia (EE).
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- congestion_income_FI_EE(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
congestion_income_FI_EE <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 48,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Unused bilateral trade capacity FI-RUS
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/unused-bilateral-trade-capacity-fi-rus
#' @return A data frame object that contains Unused bilateral trade capacity
#'     from Finland (FI) to Russia (RUS). The capacity of electricity
#'     transmission in bilateral trade can be left unused if the parties do
#'     not export the maximum amount of electricity to Russia.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- unused_biliteral_trade_capacity_FI_to_RUS(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
unused_biliteral_trade_capacity_FI_to_RUS <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 49,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Intraday transmission capacity FI-RUS
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/intraday-transmission-capacity-fi-rus
#' @return A data frame object that contains the capacity given to intraday
#'     market means transfer capacity after day-ahead trade from
#'     Finland (FI) to Russia (RUS). The indraday capacity between Finland and
#'     Russia is updated once a day. The data will not be revised after
#'     hourly day-ahead trade.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- intraday_transmission_capacity_FI_to_RUS(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
intraday_transmission_capacity_FI_to_RUS <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 50,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Procured automatic frequency restoration reserve capacity, price, down
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/procured-afrr-capacity-price-down
#' @return A data frame object that contains volume weighted average price
#'     for procured downward automatic Frequency Restoration Reserve (aFRR)
#'     capacity, in EUR/MW.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- procured_aFRR_capacity_down_price(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
procured_aFRR_capacity_down_price <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 51,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Procured a FRR capacity price, up
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/procured-a-frr-capacity-price-up
#' @return A data frame object that contains volume weighted average price
#'     for procured upward automatic Frequency Restoration Reserve (aFRR)
#'     capacity, in EUR/MW.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- procured_aFRR_capacity_up_price(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
procured_aFRR_capacity_up_price <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 52,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Activated automatic frequency restoration reserve, down
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/activated-automatic-frequency-restoration-reserve-down
#' @return A data frame object that contains activated automatic Frequency
#'     Restoration Reserve (aFRR) energy, down, in MWh.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- activated_aFRR_up(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
activated_aFRR_up <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 53,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Activated automatic frequency restoration reserve, up
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/activated-automatic-frequency-restoration-reserve-up
#' @return A data frame object that contains activated automatic Frequency
#'     Restoration Reserve (aFRR) energy, up, in MWh.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- activated_aFRR_down(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
activated_aFRR_down <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 54,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Measured electrical transmission between Finland and Estonia
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/measured-electrical-transmission-between-finland-and-estonia
#' @return A data frame object that contains measured electrical transmission
#'     between Finland and Estonia HVDC tile lines (Estlink 1 and Estlink 2).
#'     Positive sign means transmission from Finland to Estonia. Negative sign
#'     means transmission from Estonia to Finland.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- measured_electrical_transmission_FI_to_EE(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
measured_electrical_transmission_FI_to_EE <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 55,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Measured electrical transmission between Finland and Norway
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/measured-electrical-transmission-between-finland-and-norway
#' @return A data frame object that contains measured electrical transmission
#'     between Finland and Norway 220kV tie line. Positive sign means
#'     transmission from Finland to Norway. Negative sign means transmission
#'     from Norway to Finland.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- measured_electrical_transmission_FI_to_NO(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
measured_electrical_transmission_FI_to_NO <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 57,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Measured transmission of electricity between Finland and Russia
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/measured-transmission-of-electricity-between-finland-and-russia
#' @return A data frame object that contains measured electrical transmission
#'     between Finland and Russia. Positive sign means transmission from
#'     Finland to Russia. Negative sign means transmission from Russia to
#'     Finland.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- measured_electrical_transmission_FI_to_RUS(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
measured_electrical_transmission_FI_to_RUS <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 58,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Measured transmission of electiricity between Finland and Northern Sweden
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/measured-transmission-of-electiricity-between-finland-and-northern-sweden
#' @return A data frame object that contains measured transmission of
#'     electricity between Finland and Northern Sweden (SE1). Positive sign
#'     means transmission from Finland to Northern Sweden (SE1). Negative sign
#'     means transmission from Northern Sweden (SE1) to Finland.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- measured_electrical_transmission_FI_to_SE1(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
measured_electrical_transmission_FI_to_SE1 <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 60,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Measured electrical transmission between Finland and Central Sweden
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/measured-electrical-transmission-between-finland-and-central-sweden
#' @return A data frame object that contains measured electrical transmission
#'     between Finland and Central Sweden (SE3) high voltage direct current
#'     tie lines. Positive sign means transmission from Finland to Central
#'     Sweden (SE3). Negative sign means transmission from Central Sweden
#'     (SE3) to Finland.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- measured_electrical_transmission_FI_to_SE3(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
measured_electrical_transmission_FI_to_SE3 <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 61,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Transmission capacity RUS-FI
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/transmission-capacity-rus-fi
#' @return A data frame object that contains the total commercial transmission
#'     capacity of the 400 kV transmission lines from Russia to Finland owned
#'     by Fingrid.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- transmission_capacity_RUS_to_FI(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
transmission_capacity_RUS_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 63,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Unused bilateral trade capacity RUS-FI
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/unused-bilateral-trade-capacity-rus-fi
#' @return A data frame object that contains unused bilateral trade capacity
#'     from Russia (RUS) to Finland (FI). The capacity of electricity
#'     transmission in bilateral trade can be left unused if the parties do
#'     not import the maximum amount of electricity to Finland.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- unused_bilateral_trade_capacity_RUS_to_FI(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
unused_bilateral_trade_capacity_RUS_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 64,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Bilateral trade capacity RUS-FI
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/bilateral-trade-capacity-rus-fi
#' @return A data frame object that contains the bilateral capacity on the
#'     400 kV connection from Russia (RUS) to Finland (FI) that is reserved
#'     to bilateral trade of the following commercial day. The capacity is
#'     confirmed by Fingrid and the Russian parties.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- bilateral_trade_capacity_RUS_to_FI(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
bilateral_trade_capacity_RUS_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 65,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Intraday transmission capacity RUS-FI
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/intraday-transmission-capacity-rus-fi
#' @return A data frame object that contains the capacity given to intraday
#'     market means transfer capacity after day-ahead trade from Russia to
#'     Finland. The intraday capacity between Finland and Russia is updated
#'     once a day.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- intraday_transmission_capacity_RUS_to_FI(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
intraday_transmission_capacity_RUS_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 66,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Stock exchange capacity RUS-FI
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/stock-exchange-capacity-rus-fi
#' @return A data frame object that contains the capacity on the 400 kV
#'     connection from Russia to Finland is reserved to direct trade of the
#'     following commercial day. Fingrid and the Russian parties, who have
#'     jointly agreed that the capacity is 140 MW in both directions, daily
#'     confirm the capacity.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- stock_exchange_capacity_RUS_to_FI(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
stock_exchange_capacity_RUS_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 67,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Bilateral trade between Finland and Russia
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/bilateral-trade-between-finland-and-russia
#' @return A data frame object that contains bilateral trade between Finland
#'     and Russia. Fingrid and the Russian parties confirm the bilateral
#'     trades on 400 kV cross-border connection in the morning of the
#'     commercial day D for the following commercial day D+1. The confirmed
#'     bilateral trades will be bid price-independently on the electricity
#'     spot market
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- bilateral_trade_FI_to_RUS(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
bilateral_trade_FI_to_RUS <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 68,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Exchange trade between Finland and Russia
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/stock-exchange-between-finland-and-russia
#' @return A data frame object that contains direct trade volumes derive from
#'     freely placed bids in the Nordic day-ahead (Elspot) and intraday
#'     (Elbas) electricity markets. Information is updated once the day-ahead
#'     market results are public. Information on the intraday trade is updated
#'     before the operational hour.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- exchange_trade_FI_RUS(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
exchange_trade_FI_RUS <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 69,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Congestion income between FI-SE1
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/congestion-income-between-fi-se1
#' @return A data frame object that contains congestion income between
#'     Finland (FI) and Northern Sweden (SE1).
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- congestion_income_FI_SE1(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
congestion_income_FI_SE1 <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 70,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Congestion income between FI and SE3
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/congestion-income-between-fi-se3
#' @return A data frame object that contains congestion income between
#'     Finland (FI) and Central Sweden (SE3).
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- congestion_income_FI_SE3(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
congestion_income_FI_SE3 <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 71,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Electricity production in Finland
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/electricity-production-in-finland
#' @return A data frame object that contains hourly electricity production in
#'     Finland are based on Fingrid's measurements. Minor part of production
#'     which is not measured is estimated. Updated hourly.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- electricity_production_FI(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
electricity_production_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 74,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Wind power generation - hourly data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/wind-power-generation
#' @return A data frame object that contains finnish hourly wind power
#'     generation is a sum of measurements from wind parks supplied to Fingrid
#'     and of the estimate Fingrid makes from non-measured wind parks.
#'     Non-measured wind parks are about a tenth of the production capacity.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- wind_power_hourly_data(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
wind_power_hourly_data <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 75,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Frequency containment reserve for normal operation, hourly market prices
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/frequency-containment-reserve-for-normal-operation-prices
#' @return A data frame object that contains hourly prices (EUR/MW,h) of
#'     procured frequency containment reserve for normal operation (FCR-N) in
#'     Finnish hourly market for each CET-timezone day is published previous
#'     evening at 22:45 (EET).
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- FCRN_hourly_market_prices(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
FCRN_hourly_market_prices <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 79,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Frequency containment reserves for normal operation, procured volumes in hourly market
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/frequency-containment-reserves-for-normal-operation-procured-volumes-in-hourly-market
#' @return A data frame object that contains hourly volume of procured
#'     frequency containment reserve for normal operation (FCR-N) in Finnish
#'     hourly market for each CET-timezone day is published previous evening
#'     at 22:45 (EET).
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- FCRN_procured_volumes_hourly_market(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
FCRN_procured_volumes_hourly_market <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 80,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Frequency containment reserves for disturbances, hourly market prices
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/frequency-containment-reserves-for-disturbances-hourly-market-prices
#' @return A data frame object that contains hourly prices (EUR/MW,h) of
#'     procured frequency containment reserve for disturbances upwards
#'     regulation (FCR-D up) in Finnish hourly market for each CET-timezone
#'     day is published previous evening at 22:45 (EET).
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- FCRD_hourly_market_prices(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
FCRD_hourly_market_prices <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 81,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Frequency containment reserve for disturbances, procured volumes in hourly market
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/frequency-containment-reserve-for-disturbances-procured-volumes-in-hourly-market
#' @return A data frame object that contains hourly volume of procured
#'     frequency containment reserve for disturbances upwards regulation
#'     (FCR-D up) in Finnish hourly market for each CET-timezone day is
#'     published previous evening at 22:45 (EET).
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- FCRD_procured_volumes_hourly_market(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
FCRD_procured_volumes_hourly_market <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 82,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Cross-border transmission fee, import from Russia
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/cross-border-transmission-fee-import-from-russia
#' @return A data frame object that contains hourly cross-border transmission
#'     fee (dynamic tariff) for imports from Russia on Fingrid's connections.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- border_transmission_fee_RUS_to_FI(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
border_transmission_fee_RUS_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 85,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Cross-border transmission fee, export to Russia
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/cross-border-transmission-fee-export-to-russia
#' @return A data frame object that contains hourly cross-border transmission
#'     fee (dynamic tariff) for exports to Russia on Fingrid's connections.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- border_transmission_fee_FI_to_RUS(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
border_transmission_fee_FI_to_RUS <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 86,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Transmission between Finland and Northern Sweden - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/transmission-between-finland-and-northern-sweden-real-time-data
#' @return A data frame object that contains power transmission between
#'     Northern Sweden (SE1) and Finland (FI) 400kV AC tie line. Data is based
#'     on the real-time measurements in Fingrid's operation control system.
#'     Positive sign means transmission from Finland to Northern Sweden (SE1).
#'     Negative sign means transmission from Northern Sweden (SE1) to Finland.
#'     The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- transmission_FI_to_SE1_RTD(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
transmission_FI_to_SE1_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 87,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Transmission between Finland and Central Sweden - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/transmission-between-finland-and-central-sweden-real-time-data
#' @return A data frame object that contains power transmission between
#'     Central Sweden (SE3) and Finland (FI) HVDC tie lines. Data is based on
#'     the real-time measurements in Fingrid's operation control system.
#'     Positive sign means transmission from Finland to Central Sweden (SE3).
#'     Negative sign means transmission from Central Sweden (SE3) to Finland.
#'     The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- transmission_FI_to_SE3_RTD(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
transmission_FI_to_SE3_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 89,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Transmission between Sweden and Åland - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/transmission-between-sweden-and-aland-real-time-data
#' @return A data frame object that contains power transmission between Åland
#'     and Sweden based on the real-time measurements in Fingrid's operation
#'     control system. Åland is a part of SE3 (Central-Sweden) bidding zone.
#'     Positive sign means transmission from Åland to Sweden. Negative sign
#'     means transmission from Sweden to Åland. The data is updated every 3
#'     minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- transmission_Aland_to_SE_RTD(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
transmission_Aland_to_SE_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 90,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title The price of comsumption imbalance electricity
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/the-price-of-comsumption-imbalance-electricity
#' @return A data frame object that contains the price of consumption
#'     imbalance power is the price for which Fingrid both purchases imbalance
#'     power from a balance responsible party and sells it to one. In the case
#'     of regulating hour, the regulation price is used. If no regulation has
#'     been made, the Elspot FIN price is used as the purchase and selling
#'     price of consumption imbalance power. Separate consumption imbalance
#'     ended when 1.11.2021 01.00 settlement model was changed to single
#'     imbalance.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- consumption_imbalance_electricity_price(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
consumption_imbalance_electricity_price <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 92,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title The sales price of production imbalance electricity
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/the-sales-price-of-production-imbalance-electricity
#' @return A data frame object that contains the up-regulating price of the
#'     hour is the price of production imbalance power sold by Fingrid to a
#'     balance responsible party. If no up regulation has been made or if the
#'     hour has been defined as a down-regulation hour, the day ahead spot
#'     price of Finland is used as the selling price of production imbalance
#'     power. Separate production balance ended when 1.11.2021 01.00
#'     settlement model was changed to single imbalance.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- production_imbalance_electricity_sales_price(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
production_imbalance_electricity_sales_price <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 93,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title The buying price of production imbalance electricity
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/the-bying-price-of-production-imbalance-electricity
#' @return A data frame object that contains the down-regulating price of the
#'     hour is the price of production imbalance power purchased by Fingrid
#'     from a balance responsible party. If no down-regulation has been made
#'     or if the hour has been defined as an up-regulation hour, the Elspot
#'     FIN price is used as the purchase price of production imbalance power.
#'     Separate production balance ended when 1.11.2021 01.00 setllement model
#'     was changed to single imbalance.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- production_imbalance_electricity_buying_price(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
production_imbalance_electricity_buying_price <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 96,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Bilateral trade capacity FI-RUS
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/bilateral-trade-capacity-fi-rus
#' @return A data frame object that contains the bilateral capacity on the
#'     400 kV connection from Russia to Finland that is reserved to bilateral
#'     trade of the following commercial day. The capacity is confirmed by
#'     Fingrid and the Russian parties.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- biliteral_trade_capacity_FI_to_RUS(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
biliteral_trade_capacity_FI_to_RUS <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 101,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Stock exchange capacity FI-RUS
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/stock-exchange-capacity-fi-rus
#' @return A data frame object that contains the capacity on the 400 kV
#'     connection from Finland to Russia is reserved to direct trade of the
#'     following commercial day. Fingrid and the Russian parties, who have
#'     jointly agreed that the capacity is 140 MW in both directions, daily
#'     confirm the capacity.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- stock_exchange_capacity_FI_to_RUS(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
stock_exchange_capacity_FI_to_RUS <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 102,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Transmission capacity FI-RUS
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/summakapasiteetti-fi-rus
#' @return A data frame object that contains the total commercial transmission
#'     capacity of the 400 kV transmission lines from Finland to Russia owned
#'     by Fingrid. The technical capacity on 400 kV lines from Russia to
#'     Finland is 1400 MW or 1000 MW, depending whether the NWPP power plant
#'     that is located in St. Petersburg area is connected to the Finnish or
#'     the Russian power system. Fingrid has reserved 100 MW of transmission
#'     capacity from Russia to Finland to buy reserve power. The technical
#'     maximum capacity from Finland to Russia is 350 MW, of which Fingrid has
#'     reserved 30 MW to buy reserve power.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- transmission_capacity_FI_to_RUS(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
transmission_capacity_FI_to_RUS <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 103,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title The sum of the down-regulation bids in the Balancing energy market
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/the-sum-of-the-down-regualtion-bids-in-the-balancing-energy-market
#' @return A data frame object that contains the hourly sum of the
#'     down-regulation offers given by Finnish parties to the Balancing energy
#'     market is published hourly with one hour delay, eg. information from
#'     hour 07-08 is published at 9 o'clock.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- sum_down_regulation_bids_balancing_energy_market(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
sum_down_regulation_bids_balancing_energy_market <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 105,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Down-regulation price in the Balancing energy market
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/down-regulation-price-in-the-balancing-energy-market
#' @return A data frame object that contains down-regulation price in the
#'     Balancing energy market. The price of the cheapest regulating bid used
#'     in the balancing power market during the particular hour; however, at
#'     the most the price for price area Finland in Nord Pool Spot (Elspot FIN).
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- down_regulation_price_balancing_energy_market(start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
down_regulation_price_balancing_energy_market <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 106,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Transmission capacity to be given to intraday market EE-FI
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/transmission-capacity-to-be-given-to-intraday-market-ee-fi
#' @return A data frame object that contains transmission capacity to be given
#'     to intraday market EE - FI
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- transmission_capacity_to_be_given_intraday_market_EE_to_FI(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
transmission_capacity_to_be_given_intraday_market_EE_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 110,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Real time intraday transmission capacity EE-FI
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/real-time-transmission-capacity-to-be-given-to-intraday-market-ee-fi
#' @return A data frame object that contains transmission capacity to be given
#'     to intraday market EE-FI. After Elspot trades have been closed, real
#'     time intraday capacity is equivalent to the allocated intraday capacity.
#'     The real time capacity is updated after each intraday trade so that it
#'     corresponds to real time situation.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- intraday_transmission_capacity_EE_to_FI_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
intraday_transmission_capacity_EE_to_FI_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 111,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Day-ahead transmission capacity EE-FI
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/intra-day-transmission-capacity-ee-fi
#' @return A data frame object that contains day-ahead transmission capacity
#'     from Estonia (EE) to Finland (FI). Transmission capacity is given
#'     hourly for every hour of the next day. Each hour is given one value.
#'     Day-ahead transmission capacity Fingrid will publish every day in the
#'     afternoon. This capacity will not changed after publication.
#'     Transmission capacity mean the capability of the electricity system to
#'     supply electricity to the market without compromising the system
#'     security.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- day_ahead_transmission_capacity_EE_to_FI(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
day_ahead_transmission_capacity_EE_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 112,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Transmission capacity given to intraday market FI-EE
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/intraday-markkinoille-annettu-siirtokapasiteetti-ee-fi
#' @return A data frame object that contains transmission capacity to be given
#'     to intraday market FI-EE.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- transmission_capacity_intraday_market_FI_to_EE(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
transmission_capacity_intraday_market_FI_to_EE <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 113,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Real time transmission capacity available for the intraday market FI-EE
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/real-time-transmission-capacity-to-be-given-to-intraday-market-fi-ee
#' @return A data frame object that contains transmission capacity to be
#'     given to intraday market FI-EE. After Elspot trades have been closed,
#'     real time intraday capacity is equivalent to the allocated intraday
#'     capacity. The real time capacity is updated after each intraday trade
#'     so that it corresponds to real time situation.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- intraday_transmission_capacity_FI_to_EE_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
intraday_transmission_capacity_FI_to_EE_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 114,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Day-ahead transmission capacity FI-EE
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/indra-day-transmission-capacity-fi-ee
#' @return A data frame object that contains day-ahead transmission capacity
#'     from Finland (FI) to Estonia (EE). Transmission capacity is given hourly
#'     for every hour of the next day. Each hour is given one value. Day-ahead
#'     transmission capacity Fingrid will publish every day in the afternoon.
#'     This capacity will not changed after publication. Transmission capacity
#'     mean the capability of the electricity system to supply electricity to
#'     the market without compromising the system security.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- day_ahead_transmission_capacity_FI_to_EE(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
day_ahead_transmission_capacity_FI_to_EE <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 115,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Special regulation, down-regulation
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/erikoissaato-alassaato
#' @return A data frame object that contains regulation which takes place in
#'     the regulating power market by Fingrid for reasons other than the needs
#'     of national balance management
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- special_regulation_down_regulation(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
special_regulation_down_regulation <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 118,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Special regulation, up-regulation
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/special-regulation-up-regulation
#' @return A data frame object that contains regulation which takes place in
#'     the regulating power market by Fingrid for reasons other than the needs
#'     of national balance management
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- special_regulation_up_regulation(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
special_regulation_up_regulation <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 119,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Activated frequency containment reserve for normal operation
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/activated-frequency-containment-reserve-for-normal-operation
#' @return A data frame object that contains activated Frequency Containment
#'     Reserve for Normal operation (FCR-N) is published hourly one hour after
#'     the hour in question, for example the value for hour 07-08 is published
#'     at 9 o'clock.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- activated_FCRN(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
activated_FCRN <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 123,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Electricity consumption in Finland
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/electricity-consumption-in-finland
#' @return A data frame object that contains electricity consumption in
#'     Finland is based on Fingrid's production measurements. Minor part of
#'     production which is not measured is estimated. The consumption is
#'     calculated as follows: Consumption = Production + Import - Export.
#'     Updated hourly.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- electricity_consumption_FI(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
electricity_consumption_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 124,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Planned transmission capacity RUS-FI
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/planned-transmission-capacity-rus-fi
#' @return A data frame object that contains planned transmission capacity
#'     from Russia to Finland. Transmission capacity is given hourly for every
#'     next week hour.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- planned_transmission_capacity_RUS_to_FI(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
planned_transmission_capacity_RUS_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 127,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Commercial electricity flow between Finland and Estonia (FI-EE)
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/commercial-flow-fi-ee
#' @return A data frame object that contains commercial electricity flow
#'     (dayahead market and intraday market) between Finland (FI) and Estonia
#'     (EE) including system supportive trade between TSOs. Positive sign is
#'     export from Finland to Estonia.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- commercial_electricity_flow_FI_to_EE(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
commercial_electricity_flow_FI_to_EE <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 140,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Planned day-ahead transmission capacity SE1-FI
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/planned-transmission-capacity-se1-fi
#' @return A data frame object that contains planned day-ahead transmission
#'     capacity from North-Sweden (SE1) to Finland (FI). Transmission capacity
#'     is given hourly for every next week hour.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- planned_day_ahead_transmission_capacity_SE1_to_FI(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
planned_day_ahead_transmission_capacity_SE1_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 142,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Planned day-ahead transmission capacity FI-SE1
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/planned-transmission-capacity-fi-se1
#' @return A data frame object that contains planned day-ahead transmission
#'     capacity from Finland (FI) to North-Sweden (SE1). Transmission capacity
#'     is given hourly for every next week hour.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- planned_day_ahead_transmission_capacity_FI_to_SE1(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
planned_day_ahead_transmission_capacity_FI_to_SE1 <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 143,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Planned day-ahead transmission capacity SE3-FI
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/planned-day-ahead-transmission-capacity-se3-fi
#' @return A data frame object that contains planned day-ahead transmission
#'     capacity from Central-Sweden (SE3) to Finland (FI). Transmission
#'     capacity is given hourly for every next week hour.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- planned_day_ahead_transmission_capacity_SE3_to_FI(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
planned_day_ahead_transmission_capacity_SE3_to_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 144,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Planned day-ahead transmission capacity FI-SE3
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/suunniteltu-vuorokausimarkkinoille-annettava-siirtokapasiteetti-fi-se3
#' @return A data frame object that contains planned day-ahead transmission
#'     capacity from Finland (FI) to Central-Sweden (SE3). Transmission
#'     capacity is given hourly for every next week hour.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- planned_day_ahead_transmission_capacity_FI_to_SE3(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
planned_day_ahead_transmission_capacity_FI_to_SE3 <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 145,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Electricity consumption forecast for the next 24 hours.
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/an-hourly-comsumption-forecast-for-the-next-24-hours
#' @return A data frame object that contains an hourly consumption forecast
#'     for the next 24 hours made by Fingrid. Forecast is published on previous
#'     day at 12:00 EET.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- electricity_consumption_forecast_next_24h(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
electricity_consumption_forecast_next_24h <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 165,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Electricity consumption forecast of Finland. Forecast is updated hourly
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/updated-electricity-consumption-forecast-of-finland
#' @return A data frame object that contains electricity consumption forecast
#'     of Finland. The forecast is made by Fingrid and updated hourly.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- electricity_consumption_forecast_hourly_updated(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
electricity_consumption_forecast_hourly_updated <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 166,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Imbalance power between Finland and Sweden
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/imbalance-power-between-finland-and-sweden
#' @return A data frame object that contains the volume of power equals to
#'     the difference between measured and commercial transmission between
#'     Finland and Sweden. The tradetypes of commercial flow include day
#'     ahead, intraday and trades between Fingrid and Svenska Kraftnät during
#'     the operational hour. When the value of imbalance power volume is
#'     positive Fingrid has sold imbalance power to Sweden. When the value
#'     of imbalance power volume is negative Fingrid has bought imbalance power
#'     from Sweden.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_power_FI_to_SE(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
imbalance_power_FI_to_SE <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 176,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Frequency - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/frequency-real-time-data
#' @return A data frame object that contains frequency of the power system
#'     based on the real-time measurements in Fingrid's operation control
#'     system. The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- frequency_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
frequency_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 177,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Temperature in Helsinki - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/temperature-in-helsinki-real-time-data
#' @return A data frame object that contains outside air temperature
#'     measurement at Tammisto substation. The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- temperature_helsinki_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
temperature_helsinki_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 178,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Transmission between Finland and Estonia - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/transmission-between-finland-and-estonia-real-time-data
#' @return A data frame object that contains power transmission between Finland
#'     and Estonia HVDC tie lines (Estlink 1 and Estlink 2). Data is based on
#'     the real-time measurements in Fingrid's operation control system.
#'     Positive sign means transmission from Finland to Estonia. Negative sign
#'     means transmission from Estonia to Finland. The data is updated every
#'     3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- transmission_FI_to_EE_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
transmission_FI_to_EE_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 180,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Wind power production - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/wind-power-production-real-time-data
#' @return A data frame object that contains wind power production based on the
#'     real-time measurements in Fingrid's operation control system. About a
#'     tenth of the production capacity is estimated as measurements aren't
#'     available. The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- wind_power_production_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
wind_power_production_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 181,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Temperature in Jyväskylä - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/temperature-in-jyvaskyla-real-time-data
#' @return A data frame object that contains outside air temperature
#'     measurement at Petäjävesi substation. The data is updated every
#'     3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- temperature_jyvaskyla_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
temperature_jyvaskyla_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 182,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Peak load power - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/peak-load-power-real-time-data
#' @return A data frame object that contains activated peak load power based on
#'     the real-time measurements in Fingrid's operation control system
#'     including peak load reserve activations and trial runs during winter
#'     period. The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- peak_load_power_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
peak_load_power_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 183,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Temperature in Rovaniemi - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/temperature-in-rovaniemi-real-time-data
#' @return A data frame object that contains outside air temperature
#'     measurement at Valajaskoski substation. The data is updated every 3
#'     minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- temperature_rovaniemi_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
temperature_rovaniemi_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 185,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Surplus/deficit, cumulative - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/surplus-deficit-cumulative-real-time-data
#' @return A data frame object that contains power deficit/surplus represents
#'     the balance between production and consumption in Finland, taking into
#'     account imports and exports. It is calculated as the difference between
#'     the measured net import/export and the confirmed net exchange program
#'     between Finland and the other Nordic countries. The cumulative
#'     production deficit/surplus is the hourly energy generated from the
#'     difference. Sign convention: production deficit -, surplus +.
#'     The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- surplus_or_deficit_cumulative_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
surplus_or_deficit_cumulative_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 186,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Transmission between Finland and Norway - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/transmission-between-finland-and-norway-real-time-data
#' @return A data frame object that contains power transmission between Finland
#'     and Norway 220kV AC tie line. Data is based on the real-time
#'     measurements in Fingrid's operation control system. Positive sign means
#'     transmission from Finland to Norway. Negative sign means transmission
#'     from Norway to Finland. The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- transmission_FI_to_NO_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
transmission_FI_to_NO_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 187,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Nuclear power production - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/nuclear-power-production-real-time-data
#' @return A data frame object that contains nuclear power production in
#'     Finland based on the real-time measurements in Fingrid's operation
#'     control system. The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- nuclear_power_production_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
nuclear_power_production_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 188,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Condensing power production - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/condensing-power-production-real-time-data
#' @return A data frame object that contains condensing power production based
#'     on the real-time measurements in Fingrid's operation control system.
#'     The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- condensing_power_production_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
condensing_power_production_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 189,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Hydro power production - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/hydro-power-production-real-time-data
#' @return A data frame object that contains hydro power production in Finland
#'     based on the real-time measurements in Fingrid's operation control
#'     system. The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- hydro_power_production_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
hydro_power_production_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 191,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Electricity production in Finland - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/electricity-production-in-finland-real-time-data
#' @return A data frame object that contains electricity production in Finland
#'     based on the real-time measurements in Fingrid's operation control
#'     system The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- electricity_production_FI_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
electricity_production_FI_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 192,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Electricity consumption in Finland - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/electricity-consumption-in-finland-real-time-data
#' @return A data frame object that contains
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- electricity_consumption_FI_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
electricity_consumption_FI_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 193,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Net import/export of electricity - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/net-import-export-of-electricity-real-time-data
#' @return A data frame object that contains net import to Finland and net
#'     export from Finland. The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- net_import_or_export_electricity_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
net_import_or_export_electricity_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 194,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Transmission between Finland and Russia - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/transmission-between-finland-and-russia-real-time-data
#' @return A data frame object that contains power transmission between Finland
#'     and Russia based on the real-time measurements in Fingrid's operation
#'     control system. Positive sign means transmission from Finland to Russia.
#'     Negative sign means transmission from Russia to Finland. The data is
#'     updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- transmission_FI_to_RUS_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
transmission_FI_to_RUS_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 195,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Temperature in Oulu - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/temperature-in-oulu-real-time-data
#' @return A data frame object that contains outside air temperature
#'     measurement at Leväsuo substation. The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- temperature_oulu_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
temperature_oulu_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 196,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Production surplus/deficit in Finland - Real time
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check data	https://data.fingrid.fi/en/dataset/production-surplus-deficit-in-finland-real-time-data
#' @return A data frame object that contains Finland's energy production
#'     surplus/deficit. Information is based on the real time measurements in
#'     Fingrid's power control system. Power deficit/surplus represents the
#'     balance between power production and consumption in Finland, taking into
#'     account imports and exports. Power deficit/surplus is calculated as the
#'     difference between the measured net import/export and the confirmed net
#'     exchange program between Finland and the other Nordic countries. Sign
#'     convention: production deficit -, surplus +. The data is updated every
#'     3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- production_surplus_or_deficit_FI_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
production_surplus_or_deficit_FI_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 198,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Cogeneration of district heating - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/cogeneration
#' @return A data frame object that contains cogeneration of district heating
#'     based on the real-time measurements in Fingrid's operation control
#'     system. The data is updated every 3 minutes. Cogeneration means power
#'     plants that produce both electricity and district heating or process
#'     steam (combined heat and power, CHP).
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- cogeneration_district_heating_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
cogeneration_district_heating_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 201,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Industrial cogeneration - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/industrial-cogeneration-real-time-data
#' @return A data frame object that contains cogeneration of industry based on
#'     the real-time measurements in Fingrid's operation control system. The
#'     data is updated every 3 minutes. Cogeneration means power plants that
#'     produce both electricity and district heating or process steam
#'     (combined heat and power, CHP).
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- industrial_cogeneration_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
industrial_cogeneration_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 202,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Other production inc. estimated small-scale production and reserve power plants - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/gas-turbine-and-estimated-small-scale-production-real-time-data
#' @return A data frame object that contains reserve power plants electrical
#'     production is based on the real-time measurements in Fingrid's operation
#'     control system. Estimated small-scale production is added, of which
#'     there are no measurements available. The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- other_production_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
other_production_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 205,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Time deviation - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/time-deviation-real-time-data
#' @return A data frame object that contains time deviation is the time
#'     difference in seconds between a clock running according to the frequency
#'     of the grid and a reference clock independent of the frequency of the
#'     grid. The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- time_deviation_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
time_deviation_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 206,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Power system state - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/power-system-state-real-time-data
#' @return A data frame object that contains different states of the power
#'     system - traffic lights: 1=green, 2=yellow, 3=red, 4=black, 5=blue.
#'     The data is updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- power_system_state_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
power_system_state_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 209,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Other power transactions, down-regulation
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/other-power-transactions-down-regulation
#' @return A data frame object that contains other power transactions which are
#'     necessary in view of the power system.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- other_power_transactions_down_regulation(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
other_power_transactions_down_regulation <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 213,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Other power transactions, up-regulation
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/other-power-transactions-up-regulation
#' @return A data frame object that contains other power transactions which are
#'     necessary in view of the power system.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- other_power_transactions_up_regulation(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
other_power_transactions_up_regulation <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 214,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Hour change regulation, down-regulation
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/tunninvaihdesaato-alassaato
#' @return A data frame object that contains hour change regulation,
#'     down-regulation. In order to reduce problems encountered at the turn of
#'     the hour in the Nordic countries or in Finland, the planned production
#'     changes will be transferred to begin 15 minutes before or after the
#'     planned moment.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- hour_change_regulation_down_regulation(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
hour_change_regulation_down_regulation <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 239,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Hour change regulation, up-regulation
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/hour-change-regulation-up-regulation
#' @return A data frame object that contains  Hour change regulation,
#'     up-regulation. In order to reduce problems encountered at the turn of
#'     the hour in the Nordic countries or in Finland, the planned production
#'     changes will be transfered to begin 15 minutes before or after the
#'     planned moment.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- hour_change_regulation_up_regulation(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
hour_change_regulation_up_regulation <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 240,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Finland's electricity production prediction based on production plans
#'     informed to Fingrid
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/finland-electricity-production-prediction-based-on-production-plans-informed-to-fingrid
#' @return A data frame object that contains the calculation of production
#'     forecast in Finland, which is based on the production plans that balance
#'     responsible parties have reported to Fingrid. Production forecast is
#'     updated hourly.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- electricity_production_prediction_FI(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
electricity_production_prediction_FI <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 241,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title A tentative production prediction for the next 24 hours as an hourly energy
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/tentative-production-prediction-for-the-next-24-hours-as-an-hourly-energy
#' @return A data frame object that contains hourly electricity generation
#'     forecast, which is based on the production plans that balance responsible
#'     parties have reported to Fingrid. The forecast is published daily by
#'     6.00 pm for the next day, and it is not updated to match the updated
#'     production plans that balance responsible parties send to Fingrid hourly.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- electricity_production_prediction_FI_next_24h_as_hourly_energy(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
electricity_production_prediction_FI_next_24h_as_hourly_energy <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 242,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title The sum of the up-regulation bids in the balancing energy market.
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/the-sum-of-the-up-regulation-bids-in-the-balancing-energy-market
#' @return A data frame object that contains the hourly sum of the
#'     up-regulation offers given by Finnish parties to the Balancing energy
#'     market, which is published hourly with one hour delay, eg. information
#'     from hour 07-08 is published at 9 o'clock.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- sum_up_regulation_bids_balancing_energy_market(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
sum_up_regulation_bids_balancing_energy_market <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 243,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Up-regulating price in the Balancing energy market
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/up-regulating-price-in-the-balancing-energy-market
#' @return A data frame object that contains up-regulating price in Finland,
#'     which is the price of the most expensive up-regulating bid used in the
#'     Balancing energy market during the hour in question; however, it is at
#'     least the day ahead market price for the price area Finland.
#'     Up-regulating price for each hour is published hourly with one hour
#'     delay, eg. information from hour 07-08 is published at 9 o'clock.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- up_regulation_price_balancing_energy_market(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
up_regulation_price_balancing_energy_market <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 244,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Wind power generation forecast - updated hourly
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/wind-power-generation-forecast-updated-every-hour
#' @return A data frame object that contains Finnish wind power generation
#'     forecast for the next 36 hours. Updated hourly. The forecast is based
#'     on weather forecasts and data about the location, size and capacity of
#'     wind turbines. The weather data sourced from multiple providers.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- wind_power_generation_forecast_hourly_updated(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
wind_power_generation_forecast_hourly_updated <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 245,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Wind power generation forecast - updated once a day
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/wind-power-generation-forecast-updated-once-a-day
#' @return A data frame object that contains Finnish wind power generation
#'     forecasts for the next day. Forecast is updated every day at 12 p.m.
#'     EET. Length of the forecast is 36 hours. Overlapping hours are
#'     overwritten. The forecast is based on weather forecasts and data about
#'     the location, size and capacity of wind turbines. The weather data
#'     sourced from multiple providers.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- wind_power_generation_forecast_daily_updated(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
wind_power_generation_forecast_daily_updated <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 246,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Solar power generation forecast - updated once a day
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/solar-power-generation-forecast-updated-once-a-day
#' @return A data frame object that contains
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- solar_power_generation_forecast_updated_daily(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
solar_power_generation_forecast_updated_daily <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 247,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Solar power generation forecast - updated hourly
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/solar-power-generation-forecast-updated-every-hour
#' @return A data frame object that contains hourly updated solar power
#'     generation forecast for the next 36 hours. Solar forecasts are based on
#'     weather forecasts and estimates of installed PV capacity and location
#'     in Finland. Total PV capacity is based on yearly capacity statistics
#'     from the Finnish energy authority and estimates on installation rate
#'     of new capacity. Location information is a very rough estimate based
#'     on Finnish distribution grid operators information.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- solar_power_generation_forecast_updated_hourly(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
solar_power_generation_forecast_updated_hourly <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 248,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Price of last activated down-regulation bid, real-time publication
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/viimeksi-aktivoidun-alassaatotarjouksen-hinta-reaaliaikainen-julkaisu
#' @return A data frame object that contains the price of the last activated
#'     down-regulation bid. The price is published real-time when Finland is a
#'     separate regulation area.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- price_last_activated_down_regulation_bid_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
price_last_activated_down_regulation_bid_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 251,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Activated down-regulation power
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/aktivoitu-alassaatoteho
#' @return A data frame object that contains the activated downward power from
#'     balancing power market. The value is given for each 15 minutes and
#'     indicated the amount of activated power in the end of each 15 minute
#'     time period. The values are available starting from December 2018.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- activated_down_regulation_power(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
activated_down_regulation_power <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 252,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Activated up-regulation power
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/aktivoitu-ylossaatoteho
#' @return A data frame object that contains the activated upward power from
#'     balancing power market. The value is given for each 15 minutes and
#'     indicated the amount of activated power in the end of each 15 minute
#'     time period. The values are available starting from December 2018.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- activated_up_regulation_power(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
activated_up_regulation_power <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 253,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Kinetic energy of the Nordic power system - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/kinetic-energy-nordic-realtime
#' @return A data frame object that contains real-time estimate of the kinetic
#'     energy of the Nordic power system calculated by the Nordic transmission
#'     system operators. The data is updated every 1 minute. Historical data
#'     as of 2015/3/27 available.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- kinetic_energy_nordic_power_system_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
kinetic_energy_nordic_power_system_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 260,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Balancing Capacity Market results
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/saatokapasiteettimarkkinat-toteutunut-hankinta
#' @return A data frame object that contains the amount of capacity procured
#'     from the balancing capacity market, MW/week. Fingrid procures mFRR
#'     capacity throught the balancing capacity market on a weekly auction,
#'     which is held when needed.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- balancing_capacity_market_results(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
balancing_capacity_market_results <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 261,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Balancing Capacity Market price
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/saatokapasiteettimarkkinat-hinta
#' @return A data frame object that contains the price of capacity procured
#'     from the balancing capacity market, EUR/MW,h. Fingrid procures mFRR
#'     capacity throught the balancing capacity market on a weekly auction,
#'     which is held when needed.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- balancing_capacity_market_price(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
balancing_capacity_market_price <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 262,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Emission factor for electricity consumed in Finland - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/suomessa-kulutetun-sahkon-paastokerroin-reaaliaikatieto
#' @return A data frame object that contains estimate of carbon dioxide of
#'     produced electricity, which is consumed in Finland. The emissions are
#'     estimated by taking FInland's electricity production, electricity
#'     import as well as electricity export into account. The data is updated
#'     every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- emission_factor_electricity_consumed_FI_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
emission_factor_electricity_consumed_FI_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 265,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Emission factor of electricity production in Finland - real time data
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/suomen-sahkontuotannon-paastokerroin-reaaliaikatieto
#' @return A data frame object that contains near in real time calculated
#'     carbon dioxide emission estimate of electricity production in Finland.
#'     The emissions are estimated by summing each product of different
#'     electricity production type and their emission factor together, and by
#'     dividing the sum by Finland's total electricity production. The data is
#'     updated every 3 minutes.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- emission_factor_electricity_production_FI_RTD(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
emission_factor_electricity_production_FI_RTD <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 266,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Total solar production capacity used in the solar power forecast
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/total-solar-production-capacity
#' @return A data frame object that contains the total solar power production
#'     capacity used in Fingrid's solar power forecast. It is based on the
#'     small scale production statistics gathered by the Energy authority.
#'     It is also updated with estimates based on information that's provided
#'     to Fingrid.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- total_solar_production_used_in_forecast(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
total_solar_production_used_in_forecast <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 267,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Total wind production capacity used in the wind power forecast
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/total-wind-production-capacity
#' @return A data frame object that contains the total wind production
#'     capacity used in Fingrid's wind power forecast. It is based on capacity
#'     information gathered by Fingrid.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- total_wind_production_used_in_forecast(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
total_wind_production_used_in_forecast <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 268,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Balancing Capacity Market bids
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/saatokapasiteettimarkkinat-tarjoukset
#' @return A data frame object that contains the amount of bids in the
#'     balancing capacity market, MW/week. Fingrid procures mFRR capacity
#'     throught the balancing capacity market on a weekly auction, which is
#'     held when needed.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- balancing_capacity_market_bids(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
balancing_capacity_market_bids <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 270,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Fast Frequency Reserve FFR, received bids
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/nopea-taajuusreservi-tarjoukset
#' @return A data frame object that contains the volume of received Fast
#'     Frequency Reserve (FFR) bids. The volume of bids will be published
#'     22:00 (EET) on previous evening. The Fast Frequency Reserve (FFR) is
#'     procured to handle low-inertia situations. The needed volume of Fast
#'     Frequency Reserve depends on the amount of inertia in the power system
#'     and the size of the reference incident.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- FFR_received_bids(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
FFR_received_bids <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 275,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Fast Frequency Reserve FFR, procured volume
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/nopea-taajuusreservi-hankintamaara
#' @return A data frame object that contains the volume of procured Fast
#'     Frequency Reserve (FFR). The procured volume will be published 22:00
#'     (EET) on previous evening. The Fast Frequency Reserve (FFR) is procured
#'     to handle low-inertia situations. The needed volume of Fast Frequency
#'     Reserve depends on the amount of inertia in the power system and the
#'     size of the reference incident.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- FFR_procured_volume(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
FFR_procured_volume <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 276,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Fast Frequency Reserve FFR, price
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/nopea-taajuusreservi-hinta
#' @return A data frame object that contains the price of procured Fast
#'     Frequency Reserve (FFR) (EUR/MW). The price will be published 22:00 (EET)
#'     on previous evening. The price is determined by the price of the most
#'     expensive procured bid (marginal pricing). The Fast Frequency Reserve
#'     (FFR) is procured to handle low-inertia situations. The needed volume
#'     of Fast Frequency Reserve depends on the amount of inertia in the power
#'     system and the size of the reference incident.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- FFR_price(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
FFR_price <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 277,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Fast Frequency Reserve FFR, procurement forecast
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/nopea-taajuusreservi-hankintaennuste
#' @return A data frame object that contains the procurement prognosis for
#'     Fast Frequency Reserve (FFR) (MW). Fingrid procures FFR based on the
#'     procurement prognosis. The prognosis is updated once a day, typically
#'     at 11:00 (EET). The Fast Frequency Reserve (FFR) is procured to handle
#'     low-inertia situations. The needed volume of Fast Frequency Reserve
#'     depends on the amount of inertia in the power system and the size of
#'     the reference incident.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- FFR_procurement_forecast(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
FFR_procurement_forecast <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 278,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Measured electrical transmission between Finland and Åland
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/measured-electrical-transmission-between-finland-and-aland
#' @return A data frame object that contains measured electrical transmission
#'     between Finland and Åland islands DC tie line. Positive sign means
#'     transmission from Finland to Åland. Negative sign means transmission
#'     from Åland to Finland. The value is updated once a day before noon
#'     with the values of the previous day.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- measured_electrical_transmission_FI_to_Aland(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
measured_electrical_transmission_FI_to_Aland <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 280,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Frequency containment reserves for normal operation, received bids in hourly market
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/taajuusohjattu-kayttoreservi-tarjousmaarat-tuntimarkkinoilta
#' @return A data frame object that contains the volume of received Frequency
#'     Containment Reserves for Normal operation (FCR-N) bids. The volume of
#'     bids will be published 22:45 (EET) on previous evening.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- FCRN_received_bids_hourly_market(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
FCRN_received_bids_hourly_market <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 285,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Frequency containment reserve for disturbances, received bids in hourly market
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/taajuusohjattu-hairioreservi-tarjousmaarat-tuntimarkkinoilta
#' @return A data frame object that contains the volume of received frequency
#'     containment reserve for disturbances upwards regulation (FCR-D up) bids.
#'     The volume of bids will be published 22:45 (EET) on previous evening.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- FCRD_received_bids_hourly_market(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
FCRD_received_bids_hourly_market <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 286,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Frequency Containment Reserve for Normal operation, foreign trade
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/taajuusohjattu-kayttoreservi-ulkomaankauppa
#' @return A data frame object that contains the volume of the foreign trade
#'     of frequency containment reserve for normal operation (FCR-N) capacity.
#'     Positive numbers indicate import of capacity to Finland and negative
#'     numbers indicate export of capacity from Finland. The data contains the
#'     traded capacity for Sweden, Norway, Estonia and Russia. The data will be
#'     published 22:45 (EET) on previous evening.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- FCRN_foreign_trade(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
FCRN_foreign_trade <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 287,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Frequency Containment Reserve for Normal operation, yearly market plans
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/taajuusohjattu-kayttoreservi-vuosimarkkinasuunnitelmat
#' @return A data frame object that contains the hourly sum of reserve plans
#'     for frequency containment reserve for normal operation (FCR-N) in the
#'     yearly market. The data will be published 22:45 (EET) on previous
#'     evening.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- FCRN_yearly_market_plans(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
FCRN_yearly_market_plans <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 288,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Frequency containment reserves for disturbances, nordic trade
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/taajuusohjattu-hairioreservi-ulkomaankauppa
#' @return A data frame object that contains the volume of the nordic trade
#'     of frequency containment reserve for disturbances upwards regulation
#'     (FCR-D up) capacity. Positive numbers indicate import of capacity to
#'     Finland and negative numbers indicate export of capacity from Finland.
#'     The data contains the traded capacity for Sweden and Norway. The data
#'     will be published 22:45 (EET) on previous evening.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- FCRD_nordic_trade(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
FCRD_nordic_trade <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 289,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }

#' @title Frequency containment reserves for disturbances, reserve plans in the yearly market
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/taajuusohjattu-hairioreservi-vuosimarkkinasuunnitelmat
#' @return A data frame object that contains the hourly sum of reserve plans
#'     for frequency containment reserve for disturbances upwards regulation
#'     (FCR-D up) in the yearly market. The data will be published 22:45 (EET)
#'     on previous evening.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2021-01-01T00:00:00+0200"  # UTC+2 offset, Helsinki time
#' end = "2021-01-10T00:00:00+0200"    # UTC+2 offset, Helsinki time
#' key = "MY_SUPER_SECRET"
#' df <- FCRD_reserve_plans_yearly_market(
#'     start_time = start,
#'     end_time = end,
#'     user_key = key)
#' summary(df)
#' }
#' @export
FCRD_reserve_plans_yearly_market <-
  function(start_time = NA,
           end_time = NA,
           user_key = NA) {
    return(
      get_data(
        api_number = 290,
        start_time = start_time,
        end_time = end_time,
        user_key = user_key
      )
    )
  }
