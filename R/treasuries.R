#' Get Corporate/Government Treasury Balances
#'
#' The aggregate amount of the asset held on the balance sheets of publicly
#' tracked corporate treasuries or sovereign/government entities (e.g.
#' MicroStrategy-style corporate holders, or nation-state reserves).
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with a datetime column and one balance column per tracked entity (prefixed with the function's metric name), plus an \code{_aggregated} total column
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_treasury_balance_companies()
#' }

#' @rdname get_treasury_balance_companies
#' @export
get_treasury_balance_companies <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/treasuries/balance_companies"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, treasury_balance_companies=o) |>
    tidyr::unnest_wider(treasury_balance_companies, names_sep = "_") |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_treasury_balance_companies
#' @export
get_treasury_balance_governments <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/treasuries/balance_governments"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, treasury_balance_governments=o) |>
    tidyr::unnest_wider(treasury_balance_governments, names_sep = "_") |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_treasury_balance_companies
#' @export
get_treasury_balance_relative_companies <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/treasuries/balance_relative_companies"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, treasury_balance_relative_companies=o) |>
    tidyr::unnest_wider(treasury_balance_relative_companies, names_sep = "_") |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_treasury_balance_companies
#' @export
get_treasury_balance_relative_governments <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/treasuries/balance_relative_governments"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, treasury_balance_relative_governments=o) |>
    tidyr::unnest_wider(treasury_balance_relative_governments, names_sep = "_") |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Net Treasury Flows
#'
#' The net daily change in the amount of the asset held by corporate or
#' government treasuries.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with a datetime column and one net-flow column per tracked entity (prefixed with the function's metric name)
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_treasury_flows_net_companies()
#' }

#' @rdname get_treasury_flows_net_companies
#' @export
get_treasury_flows_net_companies <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/treasuries/flows_net_companies"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, treasury_flows_net_companies=o) |>
    tidyr::unnest_wider(treasury_flows_net_companies, names_sep = "_") |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_treasury_flows_net_companies
#' @export
get_treasury_flows_net_governments <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/treasuries/flows_net_governments"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, treasury_flows_net_governments=o) |>
    tidyr::unnest_wider(treasury_flows_net_governments, names_sep = "_") |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Number of Treasury Companies
#'
#' The count of publicly tracked companies holding a non-zero balance of the
#' asset on their balance sheet.
#'
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and count of treasury companies
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_treasury_companies_count()
#' }
get_treasury_companies_count <- function(asset="BTC",since=NULL,until=NULL,
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
    list(path = "v1/metrics/treasuries/companies_non_zero_count"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, treasury_companies_count=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}
