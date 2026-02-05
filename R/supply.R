#' Get Liquid Supply
#'
#' The amount of liquid supply, representing coins that are likely to be traded.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and liquid supply
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_liquid_supply()
#' }
get_liquid_supply <- function(since=NULL, until=NULL,
                              frequency="24h",
                              api_key = Sys.getenv("GN_API_KEY"),
                              as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/supply/liquid_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, liquid_supply=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Illiquid Supply
#'
#' The amount of illiquid supply, representing coins that are unlikely to be traded soon.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and illiquid supply
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_illiquid_supply()
#' }
get_illiquid_supply <- function(since=NULL, until=NULL,
                                frequency="24h",
                                api_key = Sys.getenv("GN_API_KEY"),
                                as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/supply/illiquid_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, illiquid_supply=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Long-Term Holder Supply
#'
#' The amount of supply held by long-term holders (LTH).
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and LTH supply
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_lth_supply()
#' }
get_lth_supply <- function(since=NULL, until=NULL,
                           frequency="24h",
                           api_key = Sys.getenv("GN_API_KEY"),
                           as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/supply/lth_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, lth_supply=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Short-Term Holder Supply
#'
#' The amount of supply held by short-term holders (STH).
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and STH supply
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_sth_supply()
#' }
get_sth_supply <- function(since=NULL, until=NULL,
                           frequency="24h",
                           api_key = Sys.getenv("GN_API_KEY"),
                           as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/supply/sth_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, sth_supply=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Supply in Profit
#'
#' The amount of supply that is currently in profit.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and supply in profit
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_supply_in_profit()
#' }
get_supply_in_profit <- function(since=NULL, until=NULL,
                                 frequency="24h",
                                 api_key = Sys.getenv("GN_API_KEY"),
                                 as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/supply/profit_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, supply_in_profit=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Supply in Loss
#'
#' The amount of supply that is currently in loss.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and supply in loss
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_supply_in_loss()
#' }
get_supply_in_loss <- function(since=NULL, until=NULL,
                               frequency="24h",
                               api_key = Sys.getenv("GN_API_KEY"),
                               as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/supply/loss_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, supply_in_loss=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Percent Supply in Profit
#'
#' The percentage of supply that is currently in profit.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and percent supply in profit
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_percent_supply_in_profit()
#' }
get_percent_supply_in_profit <- function(since=NULL, until=NULL,
                                         frequency="24h",
                                         api_key = Sys.getenv("GN_API_KEY"),
                                         as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/supply/profit_relative"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, percent_supply_in_profit=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#'  Get the total amount of a coin that is availble at a given moment
#'
#' @param asset The asset whose supply to get (see list on Glassnode)
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h

#' @return a tidy data frame of data
#' @export
#'
#' @examples
#' \dontrun{
#' # Need a valid API key to run
#' x <- get_circulable_supply()
#' }
get_circulable_supply <- function(asset="BTC",since=NULL,until=NULL,
                                 frequency="24h",
                                 api_key = Sys.getenv("GN_API_KEY"),
                                 as_date = TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/supply/current")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{asset}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)

}


#' Get the amount of coins that are considered lost or HODLed
#'
#' This is a calculated statistics
#'
#' @param asset Asset to examine (BTC or LTC)
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#' @param as_date A logical to return a Date not a POSIXct object for daily or lower frequency
#'
#' @return return a tibble with the date and the addresses
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_new_addresses()
#' }
get_provably_lost_coins <- function(asset="BTC", since=NULL, until=NULL, frequency="24h",
                                   api_key = Sys.getenv("GN_API_KEY"),
                                   as_date=TRUE) {
  tmp_name <- glue::glue("{asset}_provably_lost")
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/supply/provably_lost")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))

  if (as_date & (frequency %in% c("24h"))) {
    x$date <- as.Date(x$date)
  }
  return(x)
}


#' Get amount of a given asset is deposited in smart contracts
#'
#' This function returns the percentage (in decimal form) of all
#' circulable supply of the asset that is deposited in a smart contract
#'
#'
#' @param asset The asset to query. Default is ETH
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#'
#' @return a tidy data frame of data
#' @export
#'
#' @examples
#' \dontrun{
#' # Need a valid API key to run
#' x <- get_supply_contracts()
#' }
get_supply_contracts <- function(asset="ETH",since=NULL,until=NULL,
                                 api_key = Sys.getenv("GN_API_KEY")) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/distribution/supply_contracts")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{asset}} :=v) |>
    dplyr::mutate(date=as.Date(as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC")))
  return(x)

}


t <- asset <- NULL

#' Get Balances for Exchange Wallets
#'
#'
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param exchange What exchange to query. Defaults to all ("aggregated")
#' @param currency Return values in native tokens or in USD (the default)
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime, token amount, and exchange
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_exchange_balance()
#' }
get_exchange_balance <- function(asset="BTC",since=NULL,until=NULL,
                                 frequency="24h",
                                 exchange="aggregated",
                                 currency="USD",
                                 api_key = Sys.getenv("GN_API_KEY"),
                                 as_date=TRUE){
  tmp <- list("a" = asset,
                 "s" = since,
                 "u" = until,
                 "i" = frequency,
                 "e" = exchange,
                 "c" = currency,
                 "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/distribution/balance_exchanges")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{asset}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  x$exchange <- exchange
  return(x)
}
