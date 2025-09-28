#' Get Exchange Flow Metrics
#'
#' These functions retrieve exchange inflow and outflow metrics, providing insights
#' into exchange activity and capital movements between exchanges and the broader network.
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and exchange flow metrics
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_exchange_inflows()
#' y <- get_exchange_outflows()
#' z <- get_exchange_netflows()
#' }

#' @rdname get_exchange_flows
#' @export
get_exchange_inflows <- function(asset="BTC",since=NULL,until=NULL,
                                 frequency="24h",
                                 api_key = Sys.getenv("GN_API_KEY"),
                                 as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/transactions/transfers_volume_to_exchanges_mean"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, exchange_inflows=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_exchange_flows
#' @export
get_exchange_outflows <- function(asset="BTC",since=NULL,until=NULL,
                                  frequency="24h",
                                  api_key = Sys.getenv("GN_API_KEY"),
                                  as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/transactions/transfers_volume_from_exchanges_mean"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, exchange_outflows=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_exchange_flows
#' @export
get_exchange_netflows <- function(asset="BTC",since=NULL,until=NULL,
                                  frequency="24h",
                                  api_key = Sys.getenv("GN_API_KEY"),
                                  as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/transactions/transfers_volume_exchanges_net"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, exchange_netflows=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_exchange_flows
#' @export
get_exchange_deposits_count <- function(asset="BTC",since=NULL,until=NULL,
                                        frequency="24h",
                                        api_key = Sys.getenv("GN_API_KEY"),
                                        as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/transactions/transfers_to_exchanges_count"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, exchange_deposits_count=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}