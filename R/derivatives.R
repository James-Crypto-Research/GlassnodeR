#' Get Futures Open Interest
#'
#' These functions retrieve aggregate open interest across tracked futures
#' exchanges, broken out by margin type and venue. Open interest is a core
#' measure of aggregate leverage in the derivatives market.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and open interest
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_futures_open_interest()
#' }

#' @rdname get_futures_open_interest
#' @export
get_futures_open_interest <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_open_interest_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_open_interest=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_futures_open_interest
#' @export
get_futures_open_interest_perpetual <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_open_interest_perpetual_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_open_interest_perpetual=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_futures_open_interest
#' @export
get_futures_open_interest_crypto_margin <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_open_interest_crypto_margin_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_open_interest_crypto_margin=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_futures_open_interest
#' @export
get_futures_open_interest_cash_margin <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_open_interest_cash_margin_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_open_interest_cash_margin=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_futures_open_interest
#' @export
get_futures_open_interest_cme <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_cme_open_interest_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_open_interest_cme=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_futures_open_interest
#' @export
get_futures_open_interest_mcap_ratio <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_open_interest_mcap_ratio"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_open_interest_mcap_ratio=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Futures Trading Volume
#'
#' These functions retrieve daily futures trading volume across tracked
#' exchanges, including the CME-specific series and the volume/marketcap ratio.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and futures volume
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_futures_volume()
#' }

#' @rdname get_futures_volume
#' @export
get_futures_volume <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_volume_daily_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_volume=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_futures_volume
#' @export
get_futures_volume_perpetual <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_volume_daily_perpetual_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_volume_perpetual=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_futures_volume
#' @export
get_futures_volume_cme <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_cme_volume_daily_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_volume_cme=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_futures_volume
#' @export
get_futures_volume_mcap_ratio <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_volume_mcap_ratio"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_volume_mcap_ratio=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Futures Funding Rate
#'
#' The funding rate paid between longs and shorts on perpetual futures
#' contracts, a widely used proxy for market leverage and positioning bias.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and funding rate
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_futures_funding_rate()
#' }

#' @rdname get_futures_funding_rate
#' @export
get_futures_funding_rate <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_funding_rate_perpetual"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_funding_rate=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_futures_funding_rate
#' @export
get_futures_funding_rate_v2 <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_funding_rate_perpetual_v2"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_funding_rate_v2=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Futures Liquidations
#'
#' The total USD volume of long and/or short futures positions liquidated
#' over the given interval. A key indicator of deleveraging events and
#' derivatives-market stress.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and liquidation volume
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_futures_liquidations()
#' }

#' @rdname get_futures_liquidations
#' @export
get_futures_liquidations <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_liquidated_total_volume_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_liquidations=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_futures_liquidations
#' @export
get_futures_liquidations_long <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_liquidated_volume_long_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_liquidations_long=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_futures_liquidations
#' @export
get_futures_liquidations_short <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_liquidated_volume_short_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_liquidations_short=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Futures Estimated Leverage Ratio
#'
#' Open interest divided by exchange reserves, an estimate of the average
#' leverage employed by derivatives traders.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and estimated leverage ratio
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_futures_estimated_leverage_ratio()
#' }
get_futures_estimated_leverage_ratio <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_estimated_leverage_ratio"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_estimated_leverage_ratio=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Futures Annualized Rolling Basis (3 Month)
#'
#' The annualized basis (premium/discount) between 3-month futures and spot
#' price, a standard measure of the cost-of-carry / term structure used in
#' academic studies of the futures-basis trade.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and annualized 3-month basis
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_futures_annualized_basis_3m()
#' }
get_futures_annualized_basis_3m <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/futures_annualized_basis_3m"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, futures_annualized_basis_3m=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Perpetual Futures Reference Rate
#'
#' The volume-weighted reference rate for perpetual futures contracts across
#' tracked exchanges.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and reference rate
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_perpetuals_reference_rate()
#' }
get_perpetuals_reference_rate <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/derivatives/perpetuals_reference_rate"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, perpetuals_reference_rate=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}
