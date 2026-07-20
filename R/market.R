#' Get the closing price for a given token
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and price
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_closing_price()
#' }
get_closing_price <-  function(asset="BTC",since=NULL,until=NULL,
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
    list(path = glue::glue("v1/metrics/market/price_usd_close")),
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


#' Get BTC Dominance
#'
#' BTC Dominance represents the percentage of the total cryptocurrency market cap
#' that is represented by Bitcoin. It's a key metric for understanding Bitcoin's
#' relative position in the crypto market.
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and BTC dominance percentage
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_btc_dominance()
#' }
get_btc_dominance <- function(since=NULL,until=NULL,
                              frequency="24h",
                              api_key = Sys.getenv("GN_API_KEY"),
                              as_date=TRUE){
  tmp <- list("a" = "BTC",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/market/btc_dominance")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, btc_dominance=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get the Marketcap of a toke
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and marketcap
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_marketcap()
#' }
get_marketcap <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = glue::glue("v1/metrics/market/marketcap_usd")),
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


#' Get the MVRV (Market Value to Realized Value) Ratio
#'
#' The MVRV ratio represents the ratio between market cap and realized cap.
#' It provides insight into whether an asset is over or undervalued relative to its "fair value".
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and MVRV ratio
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_mvrv()
#' }
get_mvrv <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = glue::glue("v1/metrics/market/mvrv")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, mvrv=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get the MVRV Z-Score
#'
#' The MVRV Z-Score evaluates if an asset is over or undervalued relative to its "fair value".
#' It represents the standard deviation of the current MVRV from its historical mean.
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and MVRV Z-Score
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_mvrv_z_score()
#' }
get_mvrv_z_score <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = glue::glue("v1/metrics/market/mvrv_z_score")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, mvrv_z_score=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Median Realized Price
#'
#' The Median Realized Price represents the median acquisition cost across the total supply.
#' It provides insight into the median price at which all coins in circulation were last moved.
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and median realized price
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_realized_price()
#' }
get_realized_price <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = glue::glue("v1/metrics/market/price_realized_median_usd")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, realized_price=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Realized Cap
#'
#' Realized Cap represents the aggregate USD value of Bitcoin based on the last
#' time each coin moved, providing a more nuanced view of market valuation.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and realized cap
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_realized_cap()
#' }
get_realized_cap <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/market/marketcap_realized_usd"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, realized_cap=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Delta Cap
#'
#' Delta Cap represents the difference between Market Cap and Realized Cap.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and delta cap
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_delta_cap()
#' }
get_delta_cap <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/market/deltacap_usd"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, delta_cap=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Price Drawdown from ATH
#'
#' The relative price drawdown from the all-time high.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and price drawdown
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_price_drawdown_ath()
#' }
get_price_drawdown_ath <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/market/price_drawdown_relative"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, price_drawdown_ath=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get LTH-MVRV (Long-Term Holder MVRV)
#'
#' MVRV ratio for Long-Term Holders (held for more than 155 days).
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and LTH MVRV
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_lth_mvrv()
#' }
get_lth_mvrv <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/market/mvrv_more_155"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, lth_mvrv=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Median MVRV
#'
#' The median Market Value to Realized Value ratio.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and median MVRV
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_median_mvrv()
#' }
get_median_mvrv <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/market/mvrv_median"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, median_mvrv=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Price OHLC
#'
#' Open, High, Low, Close price data.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and the open, high, low, and close prices
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_price_ohlc()
#' }
get_price_ohlc <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/market/price_usd_ohlc"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t) |>
    tidyr::unnest_wider(o) |>
    dplyr::rename(open=o, high=h, low=l, close=c) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Realized Price
#'
#' Realized Price represents the aggregate acquisition cost basis across the
#' total supply (Realized Cap divided by circulating supply). It differs from
#' \code{get_realized_price()}, which returns the median rather than the
#' aggregate realized price.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and realized price
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_realized_price_usd()
#' }
get_realized_price_usd <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/market/price_realized_usd"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, realized_price_usd=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get STH-MVRV (Short-Term Holder MVRV)
#'
#' MVRV ratio for Short-Term Holders (held for less than 155 days). Complements
#' \code{get_lth_mvrv()}.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and STH MVRV
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_sth_mvrv()
#' }
get_sth_mvrv <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/market/mvrv_less_155"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, sth_mvrv=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Realized Volatility
#'
#' Realized (historical) price volatility computed over a rolling window. These
#' functions are grouped by window length.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and realized volatility. \code{get_realized_volatility_all()} instead returns one \code{realized_volatility_<window>} column per window (1w, 2w, 1m, 3m, 6m, 1y)
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_realized_volatility_1_month()
#' }

#' @rdname get_realized_volatility_1_week
#' @export
get_realized_volatility_1_week <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/market/realized_volatility_1_week"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, realized_volatility=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_realized_volatility_1_week
#' @export
get_realized_volatility_2_weeks <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/market/realized_volatility_2_weeks"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, realized_volatility=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_realized_volatility_1_week
#' @export
get_realized_volatility_1_month <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/market/realized_volatility_1_month"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, realized_volatility=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_realized_volatility_1_week
#' @export
get_realized_volatility_3_months <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/market/realized_volatility_3_months"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, realized_volatility=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_realized_volatility_1_week
#' @export
get_realized_volatility_6_months <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/market/realized_volatility_6_months"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, realized_volatility=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_realized_volatility_1_week
#' @export
get_realized_volatility_1_year <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/market/realized_volatility_1_year"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, realized_volatility=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_realized_volatility_1_week
#' @export
get_realized_volatility_all <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/market/realized_volatility_all"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, realized_volatility=o) |>
    tidyr::unnest_wider(realized_volatility, names_sep = "_") |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Price Correlation with BTC
#'
#' The 7-day rolling Pearson correlation between an asset's price and BTC's price.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and correlation to BTC
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_price_correlation()
#' }
get_price_correlation <- function(asset="ETH",since=NULL,until=NULL,
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
    list(path = "v1/metrics/market/correlation_btc_7d"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, correlation_btc_7d=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Price Beta to BTC
#'
#' The 7-day rolling beta of an asset's price relative to BTC's price.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and beta to BTC
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_price_beta()
#' }
get_price_beta <- function(asset="ETH",since=NULL,until=NULL,
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
    list(path = "v1/metrics/market/beta_btc_7d"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, beta_btc_7d=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Spot Trading Volume
#'
#' The total daily spot trading volume across tracked exchanges.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and spot volume
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_spot_volume()
#' }
get_spot_volume <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/market/spot_volume_daily_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, spot_volume=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Spot Volume to Marketcap Ratio
#'
#' The ratio of daily spot trading volume to market capitalization, a measure of
#' relative liquidity/turnover.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and spot volume/marketcap ratio
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_spot_volume_mcap_ratio()
#' }
get_spot_volume_mcap_ratio <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/market/spot_volume_mcap_ratio"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, spot_volume_mcap_ratio=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}