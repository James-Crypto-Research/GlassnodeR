# Indicator metrics (SSR, SSR Oscillator, Velocity)

#' Get Stablecoin Supply Ratio (SSR)
#'
#' The Stablecoin Supply Ratio (SSR) is the ratio between Bitcoin's market cap
#' and the aggregate market cap of tracked stablecoins. Lower values indicate
#' greater relative stablecoin buying power ("dry powder").
#'
#' @param since,until A POSIX compatible date-time object or date string
#' @param frequency A resolution for the data (defaults to \code{"24h"})
#' @param api_key The API key to use. By default it checks the GN_API_KEY
#'   environment variable
#' @param as_date A logical to return a Date object for daily observations
#'
#' @return A tibble with columns \code{date} and \code{ssr}
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_ssr()
#' }
get_ssr <- function(since = NULL, until = NULL,
                    frequency = "24h",
                    api_key = Sys.getenv("GN_API_KEY"),
                    as_date = TRUE) {
  tmp <- list("a" = "BTC",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/indicators/ssr"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date = t, ssr = v) |>
    dplyr::mutate(date = as.POSIXct(date, origin = "1970-01-01 00:00:00", tz = "UTC"))
  if (as_date && frequency == "24h") {
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Stablecoin Supply Ratio (SSR) Oscillator
#'
#' Tracks how the 200-day simple moving average of the Stablecoin Supply Ratio
#' moves within its Bollinger Bands to evaluate stablecoin buying-power
#' extremes.
#'
#' @param since,until A POSIX compatible date-time object or date string
#' @param frequency A resolution for the data (defaults to \code{"24h"})
#' @param api_key The API key to use. By default it checks the GN_API_KEY
#'   environment variable
#' @param as_date A logical to return a Date object for daily observations
#'
#' @return A tibble with columns \code{date} and \code{ssr_oscillator}
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_ssr_oscillator()
#' }
get_ssr_oscillator <- function(since = NULL, until = NULL,
                               frequency = "24h",
                               api_key = Sys.getenv("GN_API_KEY"),
                               as_date = TRUE) {
  tmp <- list("a" = "BTC",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/indicators/ssr_oscillator"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date = t, ssr_oscillator = v) |>
    dplyr::mutate(date = as.POSIXct(date, origin = "1970-01-01 00:00:00", tz = "UTC"))
  if (as_date && frequency == "24h") {
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Network Velocity
#'
#' Measures the rate at which units of an asset circulate through the network
#' (transfer volume relative to circulating supply). Useful for distinguishing
#' active payment/settlement usage of stablecoins from idle holding.
#'
#' @param asset Asset ticker to query (defaults to \code{"USDT"})
#' @param since,until A POSIX compatible date-time object or date string
#' @param frequency A resolution for the data (defaults to \code{"24h"})
#' @param api_key The API key to use. By default it checks the GN_API_KEY
#'   environment variable
#' @param as_date A logical to return a Date object for daily observations
#'
#' @return A tibble with columns \code{date} and \code{velocity}
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_velocity(asset = "USDC")
#' }
get_velocity <- function(asset = "USDT", since = NULL, until = NULL,
                         frequency = "24h",
                         api_key = Sys.getenv("GN_API_KEY"),
                         as_date = TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/indicators/velocity"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date = t, velocity = v) |>
    dplyr::mutate(date = as.POSIXct(date, origin = "1970-01-01 00:00:00", tz = "UTC"))
  if (as_date && frequency == "24h") {
    x$date <- as.Date(x$date)
  }
  return(x)
}
