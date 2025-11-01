#' Get Exchange Net Position Change
#'
#' The net change in exchange positions over time.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and exchange net position change
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_exchange_net_position_change()
#' }
get_exchange_net_position_change <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/distribution/exchange_net_position_change"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, exchange_net_position_change=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Miner Balance
#'
#' The total balance held by miners.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and miner balance
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_miner_balance()
#' }
get_miner_balance <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/distribution/balance_miners"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, miner_balance=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Miner Net Position Change
#'
#' The net change in miner positions over time.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and miner net position change
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_miner_net_position_change()
#' }
get_miner_net_position_change <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/distribution/miner_net_position_change"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, miner_net_position_change=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Gini Coefficient
#'
#' A measure of wealth distribution inequality.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and Gini coefficient
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_gini_coefficient()
#' }
get_gini_coefficient <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/distribution/gini"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, gini_coefficient=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Herfindahl Index
#'
#' A measure of market concentration among addresses.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and Herfindahl index
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_herfindahl_index()
#' }
get_herfindahl_index <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/distribution/herfindahl"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, herfindahl_index=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Return the HODL waves for BTC or ETH
#'
#' @param asset
#' @param since
#' @param until
#' @param api_key
#'
#' @return a Tibble with the waves as columns
#' @export
#'
#' @examples
#' x <- get_hodl_wave()
get_hodl_wave <- function(asset="BTC",since=NULL,until=NULL,
                                  api_key = Sys.getenv("GN_API_KEY")) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = "24h",
              "api_key" = api_key)
  tmp_name <- glue::glue({{asset}},"_tx_rate")
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/supply/hodl_waves")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  y <- x$o
  y$date <- as.Date(x$date)
  x <- y |> relocate(date) |>
    pivot_longer(-date,names_to="duration", values_to="amount") |>
    mutate(amount = replace_na(amount, 0)) |>
    mutate(duration = as_factor(duration)) |>
    mutate(duration = fct_relevel(duration,
                                  "more_10y","7y_10y","5y_7y",
                                  "3y_5y","2y_3y","1y_2y",
                                  "6m_12m","3m_6m","1m_3m",
                                  "1w_1m","1d_1w","24h"))
  return(x)

}


t <- asset <- NULL

#' Get the percent of circulable supply for a crypto that is at deposited in
#' large accoints
#'
#' This function calls top 1% balance call. It returns a tibble with the amount
#' that is the percent (in decimal form) of the crypto in wallets containing
#' the largest top 1% of balances. Smart contracts, exchanges and other identified
#' wallets are excluded.
#'
#' NOTE: FOr BTC this is entities (cluster of wallets) for others this is wallets
#'
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime, and percent
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_percent_top_balance()
#' }
get_percent_top_balance <- function(asset="BTC",since=NULL,until=NULL,
                                     frequency="24h",
                                     api_key = Sys.getenv("GN_API_KEY"),
                                     as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  var_name <- glue::glue({{asset}},"_pct")
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/distribution/balance_1pct_holders")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{var_name}} := v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}


t <- asset <- NULL

#' Get the percent of circulable supply for a crypto that is at exchanges
#'
#' This function calls the stacked API call. It returns a tibble with the amount
#' that is the percent (in decimal form) of the crypto in exchange wallets
#'
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime, and percent
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_percent_exchange_balances()
#' }
get_percent_exchange_balance <- function(asset="BTC",since=NULL,until=NULL,
                                     frequency="24h",
                                     api_key = Sys.getenv("GN_API_KEY"),
                                     as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  var_name <- glue::glue({{asset}},"_pct")
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/distribution/balance_exchanges_relative")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{var_name}} := v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}


t <- o <- NULL

#' Get Balances for all Exchange Wallets for a given crypto
#'
#' This function calls the stacked API call. It returns a tibble with the amount
#' in all available exchange wallets
#'
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param currency Return values in native tokens or in USD (the default)
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime, and each exchange
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_all_exchange_balances()
#' }
get_all_exchange_balance <- function(asset="BTC",since=NULL,until=NULL,
                                 frequency="24h",
                                 currency="USD",
                                 api_key = Sys.getenv("GN_API_KEY"),
                                 as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "c" = currency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/distribution/balance_exchanges_all")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t) |>
    tidyr::unnest(cols=c(o)) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}
