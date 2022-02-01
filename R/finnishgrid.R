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
#' @export
unused_biliteral_trade_capacity_RUS_to_FI <-
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
#' @export
biliteral_trade_capacity_RUS_to_FI <-
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
#' @export
biliteral_trade_FI_to_RUS <-
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

#' @title Finland's electricity production prediction based on production plans informed to Fingrid
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/finland-electricity-production-prediction-based-on-production-plans-informed-to-fingrid
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

#' @title Balancing Capacity Market proce
#' @param start_time Start time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param end_time End time in UTC with offset. Character array in ISO8601,
#'     YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key.
#'     Free from https://data.fingrid.fi/open-data-forms/registration/
#' @description Check https://data.fingrid.fi/en/dataset/saatokapasiteettimarkkinat-hinta
#' @export
balancing_capacity_market_proce <-
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
