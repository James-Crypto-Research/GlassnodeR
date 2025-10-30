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
  x <- call_glassnode_api(
    path = "v1/metrics/supply/liquid", params
  ) |>
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
  x <- call_glassnode_api(
    path = "v1/metrics/supply/illiquid", params
  ) |>
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
  x <- call_glassnode_api(
    path = "v1/metrics/supply/lth", params
  ) |>
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
  x <- call_glassnode_api(
    path = "v1/metrics/supply/sth", params
  ) |>
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
  x <- call_glassnode_api(
    path = "v1/metrics/supply/profit", params
  ) |>
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
  x <- call_glassnode_api(
    path = "v1/metrics/supply/loss", params
  ) |>
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
  x <- call_glassnode_api(
    path = "v1/metrics/supply/profit_relative", params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, percent_supply_in_profit=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}