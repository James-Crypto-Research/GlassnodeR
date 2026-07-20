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
#' @return Returns a tibble with a datetime column and \code{provably_lost_burn}, \code{provably_lost_cumsum}, \code{provably_lost_op_return}, and \code{provably_lost_unclaimed} columns
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_provably_lost_coins()
#' }
get_provably_lost_coins <- function(asset="BTC", since=NULL, until=NULL, frequency="24h",
                                   api_key = Sys.getenv("GN_API_KEY"),
                                   as_date=TRUE) {
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
    dplyr::rename(date=t, provably_lost=o) |>
    tidyr::unnest_wider(provably_lost, names_sep = "_") |>
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

#' Get Supply Inflation Rate
#'
#' The annualized inflation rate of the asset's supply, i.e. newly issued supply
#' as a percentage of circulating supply. Central to comparing the monetary
#' policy of different assets (e.g. BTC's fixed disinflationary schedule vs.
#' ETH's post-merge issuance/burn dynamics).
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and inflation rate
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_inflation_rate()
#' }
get_inflation_rate <- function(asset="BTC",since=NULL,until=NULL,
                               frequency="24h",
                               api_key = Sys.getenv("GN_API_KEY"),
                               as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/supply/inflation_rate"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, inflation_rate=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Newly Issued Supply
#'
#' The amount of new supply issued (e.g. via block rewards) over the given interval.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and issued supply
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_issued_supply()
#' }
get_issued_supply <- function(asset="BTC",since=NULL,until=NULL,
                              frequency="24h",
                              api_key = Sys.getenv("GN_API_KEY"),
                              as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/supply/issued"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, issued_supply=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Minted Supply
#'
#' The amount of new supply minted over the given interval (relevant for
#' smart-contract assets and stablecoins).
#'
#' @param asset This is the asset to get information on. As of this metric's introduction, Glassnode only supports \code{"ETH"} for it
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and minted supply
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_minted_supply()
#' }
get_minted_supply <- function(asset="ETH",since=NULL,until=NULL,
                              frequency="24h",
                              api_key = Sys.getenv("GN_API_KEY"),
                              as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/supply/minted"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, minted_supply=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Burned Supply
#'
#' The cumulative amount of supply burned (relevant for EIP-1559 assets like
#' ETH). Use \code{get_burn_rate()} for the flow (per-interval) version.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and burned supply
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_burned_supply()
#' }
get_burned_supply <- function(asset="ETH",since=NULL,until=NULL,
                              frequency="24h",
                              api_key = Sys.getenv("GN_API_KEY"),
                              as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/supply/burned"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, burned_supply=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Burn Rate
#'
#' The rate at which supply is being burned per interval (relevant for
#' EIP-1559 assets like ETH).
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and burn rate
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_burn_rate()
#' }
get_burn_rate <- function(asset="ETH",since=NULL,until=NULL,
                          frequency="24h",
                          api_key = Sys.getenv("GN_API_KEY"),
                          as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/supply/burn_rate"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, burn_rate=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Total Minted Supply
#'
#' The cumulative total supply ever minted (as distinct from the current
#' circulating supply returned by \code{get_circulable_supply()}, which nets
#' out burns).
#'
#' @param asset This is the asset to get information on. This metric covers ERC-20 tokens and stablecoins rather than BTC/ETH; the list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with a datetime column, a \code{supply_aggregated} column (the cross-chain total), and one \code{supply_<chain>} column per chain the asset is minted on
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_total_minted_supply()
#' }
get_total_minted_supply <- function(asset="USDC",since=NULL,until=NULL,
                                    frequency="24h",
                                    api_key = Sys.getenv("GN_API_KEY"),
                                    as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/supply/total_minted_supply"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, supply=o) |>
    tidyr::unnest_wider(supply, names_sep = "_") |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Illiquid/Liquid Supply Change
#'
#' The 30-day net change in illiquid or liquid supply, i.e. the flow rather
#' than the stock versions of \code{get_illiquid_supply()}/\code{get_liquid_supply()}.
#'
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and supply change
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_illiquid_supply_change()
#' }

#' @rdname get_illiquid_supply_change
#' @export
get_illiquid_supply_change <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/supply/illiquid_change"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, illiquid_supply_change=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_illiquid_supply_change
#' @export
get_liquid_supply_change <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/supply/liquid_change"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, liquid_supply_change=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Highly Liquid Supply
#'
#' The amount of supply held by entities with a very high historical
#' propensity to spend, a stricter subset of \code{get_liquid_supply()}.
#'
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and highly liquid supply
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_highly_liquid_supply()
#' }
get_highly_liquid_supply <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/supply/highly_liquid_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, highly_liquid_supply=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Supply-Adjusted Current Supply
#'
#' The current circulating supply, adjusted to exclude coins identified as lost.
#' Complements \code{get_circulable_supply()}.
#'
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and adjusted current supply
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_current_supply_adjusted()
#' }
get_current_supply_adjusted <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/supply/current_adjusted"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, current_supply_adjusted=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

duration <- value <- NULL

#' Get Realized Cap HODL Waves
#'
#' The realized-cap-weighted analogue of \code{get_hodl_wave()}: the share of
#' realized value held in coins last moved within each age band. Weighting by
#' realized value (rather than raw coin count) better reflects the economic
#' significance of each holding-period cohort.
#'
#' @param asset Asset to examine (BTC or LTC)
#' @param since The start date
#' @param until The end date
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#'
#' @return a tibble in long format with columns date, duration, and value (share of realized cap)
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_rcap_hodl_waves()
#' }
get_rcap_hodl_waves <- function(asset="BTC", since=NULL, until=NULL,
                                api_key = Sys.getenv("GN_API_KEY")) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = "24h",
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/supply/rcap_hodl_waves"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  y <- x$o
  y$date <- as.Date(x$date)
  x <- y |> dplyr::relocate(date) |>
    tidyr::pivot_longer(-date, names_to="duration", values_to="value") |>
    dplyr::mutate(value = tidyr::replace_na(value, 0)) |>
    dplyr::mutate(duration = forcats::as_factor(duration)) |>
    dplyr::mutate(duration = forcats::fct_relevel(duration,
                                  "more_10y","7y_10y","5y_7y",
                                  "3y_5y","2y_3y","1y_2y",
                                  "6m_12m","3m_6m","1m_3m",
                                  "1w_1m","1d_1w","24h"))
  return(x)
}
