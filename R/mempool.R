#' Get Mempool Metrics
#'
#' These functions retrieve various mempool-related metrics showing transactions
#' waiting to be confirmed. They provide insights into network congestion and
#' transaction fee market dynamics.
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and mempool metrics
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_mempool_count()
#' y <- get_mempool_size()
#' z <- get_mempool_fees()
#' }

#' @rdname get_mempool
#' @export
get_mempool_count <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = glue::glue("v1/metrics/mempool/txs_count_sum")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, mempool_count=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_mempool
#' @export
get_mempool_size <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = glue::glue("v1/metrics/mempool/txs_size_sum")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, mempool_size=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_mempool
#' @export
get_mempool_fees <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = glue::glue("v1/metrics/mempool/fees_sum")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, mempool_fees=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_mempool
#' @export
get_mempool_value <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = glue::glue("v1/metrics/mempool/txs_value_sum")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, mempool_value=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Mempool Average Relative Fee
#'
#' The mean relative fee of transactions waiting in the mempool.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and average relative fee
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_mempool_fees_average_relative()
#' }
get_mempool_fees_average_relative <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/mempool/fees_average_relative"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, fees_average_relative=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Mempool Median Relative Fee
#'
#' The median relative fee of transactions waiting in the mempool.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and median relative fee
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_mempool_fees_median_relative()
#' }
get_mempool_fees_median_relative <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/mempool/fees_median_relative"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, fees_median_relative=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Mempool Value Distribution by Relative Fee
#'
#' The total amount of coins in transactions waiting in different relative fee cohorts.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and value distribution
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_mempool_value_distribution()
#' }
get_mempool_value_distribution <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/mempool/txs_value_distribution"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, value_distribution=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Mempool Fee Distribution by Relative Fee
#'
#' The total amount of fees of transactions waiting in different relative fee cohorts.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and fee distribution
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_mempool_fees_distribution()
#' }
get_mempool_fees_distribution <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/mempool/fees_distribution"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, fees_distribution=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Mempool Transaction Count Distribution by Relative Fee
#'
#' The total number of transactions waiting in different relative fee cohorts.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and transaction count distribution
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_mempool_txs_count_distribution()
#' }
get_mempool_txs_count_distribution <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/mempool/txs_count_distribution"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, txs_count_distribution=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Mempool Transaction Size Distribution by Relative Fee
#'
#' The size of transactions waiting in different relative fee cohorts.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and transaction size distribution
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_mempool_txs_size_distribution()
#' }
get_mempool_txs_size_distribution <- function(since=NULL, until=NULL,
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
    list(path = "v1/metrics/mempool/txs_size_distribution"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, txs_size_distribution=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}