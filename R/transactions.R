#' Get the total number of transactions in the given frequency
#'
#' @param asset The chain to examine. Limited to BTC, ETH, LTC
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h,10m,1w,1month
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#' @param as_date A logical to return a Date not a POSIXct object for daily or lower frequency
#'
#' @return This function returns a tibble consisting of the date and transaction rates.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_transactions_count()
#' }
get_transactions_count <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                 api_key = Sys.getenv("GN_API_KEY"),
                                 as_date=TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  tmp_name <- glue::glue({{asset}},"_tx_count")
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/transactions/count")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & (frequency %in% c("24h", "1w","1month"))) {
    x$date <- as.Date(x$date)
  }
  return(x)

}


#' Get the total rate of transactions per second in the given frequency
#'
#' @param asset The chain to examine. Limited to BTC, ETH, LTC
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h,10m,1w,1month
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#' @param as_date A logical to return a Date not a POSIXct object for daily or lower frequency
#'
#' @return This function returns a tibble consisting of the date and transaction rates.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_transactions_rate()
#' }
get_transactions_rate <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                 api_key = Sys.getenv("GN_API_KEY"),
                                 as_date=TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  tmp_name <- glue::glue({{asset}},"_tx_rate")
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/transactions/rate")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & (frequency %in% c("24h", "1w","1month"))) {
    x$date <- as.Date(x$date)
  }
  return(x)

}


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
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/transactions/transfers_volume_to_exchanges_mean")),
    params
  )) |>
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
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/transactions/transfers_volume_from_exchanges_mean")),
    params
  )) |>
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
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/transactions/transfers_volume_exchanges_net")),
    params
  )) |>
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
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/transactions/transfers_to_exchanges_count")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, exchange_deposits_count=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Transaction Volume Metrics
#'
#' These functions retrieve various transaction volume metrics including
#' change-adjusted volumes and entity-adjusted volumes that provide insights
#' into economic activity on the blockchain.
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and volume metrics
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_volume_adjusted_mean()
#' y <- get_volume_adjusted_total()
#' z <- get_volume_entity_adjusted()
#' }

#' @rdname get_transaction_volumes
#' @export
get_volume_adjusted_mean <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = glue::glue("v1/metrics/transactions/transfers_volume_adjusted_mean")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, volume_adjusted_mean=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_transaction_volumes
#' @export
get_volume_adjusted_median <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = glue::glue("v1/metrics/transactions/transfers_volume_adjusted_median")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, volume_adjusted_median=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_transaction_volumes
#' @export
get_volume_adjusted_total <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = glue::glue("v1/metrics/transactions/transfers_volume_adjusted_sum")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, volume_adjusted_total=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_transaction_volumes
#' @export
get_volume_entity_adjusted <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = glue::glue("v1/metrics/transactions/transfers_volume_entity_adjusted_sum")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, volume_entity_adjusted=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}