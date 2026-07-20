#' Get US Spot ETF Balances (Latest)
#'
#' The most recent aggregate BTC/ETH balance held by US spot ETFs, broken
#' down per ETF ticker (plus a \code{total} column).
#'
#' This is a snapshot-only endpoint: Glassnode always returns just the
#' latest observation for it, so \code{since}/\code{until} are accepted for
#' interface consistency with the rest of the package but have no effect.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until Ignored. Accepted for interface consistency; this endpoint only ever returns the latest snapshot
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with a datetime column and one balance column per ETF ticker (prefixed \code{balance_}) and one change column per ticker (prefixed \code{changes_})
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_etf_balances()
#' }
get_etf_balances <- function(asset="BTC",since=NULL,until=NULL,
                             frequency="24h",
                             api_key = Sys.getenv("GN_API_KEY"),
                             as_date=TRUE){
  tmp <- list("a" = asset,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/institutions/us_spot_etf_balances_latest"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t) |>
    tidyr::unnest_wider(balance, names_sep = "_") |>
    tidyr::unnest_wider(changes, names_sep = "_") |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get US Spot ETF Net Flows
#'
#' The net daily USD flow (creations minus redemptions) across US spot ETFs.
#' A widely watched gauge of institutional demand relevant to policy analysis
#' of the post-2024 spot ETF regime.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and net ETF flows
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_etf_flows_net()
#' }
get_etf_flows_net <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/institutions/us_spot_etf_flows_net"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, etf_flows_net=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get US Spot ETF Price
#'
#' The reference price used for US spot ETF valuation, broken down per ETF
#' ticker.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with a datetime column and one price column per ETF ticker (prefixed \code{price_})
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_etf_price()
#' }
get_etf_price <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/institutions/us_spot_etf_price"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, price=o) |>
    tidyr::unnest_wider(price, names_sep = "_") |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get US Spot ETF Total Trading Volume
#'
#' The aggregate daily trading volume across all US spot ETFs.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and total ETF volume
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_etf_volume_total()
#' }
get_etf_volume_total <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/institutions/us_spot_etf_volume_total"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, etf_volume_total=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}
