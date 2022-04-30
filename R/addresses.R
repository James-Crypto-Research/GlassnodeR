# This contains all the address functions

#' Get all addresses statistics during the period
#'
#' @param asset Asset to examine (see glassnode api docs for a list)
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h,10m,1w,1month
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#' @param as_date A logical to return a Date not a POSIXct object for daily or lower frequency
#'
#' @return return a tibble with the date and the addresses
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_addresses()
#' }
get_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                          api_key = Sys.getenv("GN_API_KEY"),
                          as_date=TRUE) {
  active_a <-get_active_addresses(asset=asset,since=since,until=until,
                                  frequency=frequency,api_key=api_key,
                                  as_date=as_date)
  total_a <-get_total_addresses(asset=asset,since=since,until=until,
                                frequency=frequency,api_key=api_key,
                                as_date=as_date)
  send_a<-get_sending_addresses(asset=asset,since=since,until=until,
                                frequency=frequency,api_key=api_key,
                                as_date=as_date)
  rec_a <-get_receiving_addresses(asset=asset,since=since,until=until,
                                  frequency=frequency,api_key=api_key,
                                  as_date=as_date)
  dep_a <-get_depositing_addresses(asset=asset,since=since,until=until,
                                   frequency=frequency,api_key=api_key,
                                   as_date=as_date)
  new_a <- get_new_addresses(asset=asset,since=since,until=until,
                             frequency=frequency,api_key=api_key,as_date=as_date)
  with_a <-get_withdrawing_addresses(asset=asset,since=since,until=until,
                                     frequency=frequency,api_key=api_key,
                                     as_date=as_date)
  x <- plyr::join_all(list(active_a,total_a,send_a,rec_a,dep_a,with_a,new_a),
                      by="date") |> tibble::as_tibble()
  return(x)

}


#' Get the number of unique addresses that withdrew funds to an exchange
#'
#' @param asset Asset to examine (see glassnode api docs for a list)
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h,10m,1w,1month
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#' @param as_date A logical to return a Date not a POSIXct object for daily or lower frequency
#'
#' @return return a tibble with the date and the addresses
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_withdrawing_addresses()
#' }
get_withdrawing_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                      api_key = Sys.getenv("GN_API_KEY"),
                                      as_date=TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  tmp_name <- glue::glue({{asset}},"_with_add")
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/addresses/receiving_from_exchanges_count"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00"))
  if (as_date & (frequency %in% c("24h", "1w","1month"))) {
    x$date <- as.Date(x$date)
  }
  return(x)

}


#' Get the number of new addresses that were active during the period
#'
#' @param asset Asset to examine (see glassnode api docs for a list)
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h,10m,1w,1month
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#' @param as_date A logical to return a Date not a POSIXct object for daily or lower frequency
#'
#' @return return a tibble with the date and the addresses
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_new_addresses()
#' }
get_new_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                              api_key = Sys.getenv("GN_API_KEY"),
                              as_date=TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  tmp_name <- glue::glue({{asset}},"_new_add")
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/addresses/new_non_zero_count"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00"))
  if (as_date & (frequency %in% c("24h", "1w","1month"))) {
    x$date <- as.Date(x$date)
  }
  return(x)

}


#' Get the number of unique addresses that sent funds to an exchange
#'
#' @param asset Asset to examine (see glassnode api docs for a list)
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h,10m,1w,1month
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#' @param as_date A logical to return a Date not a POSIXct object for daily or lower frequency
#'
#' @return return a tibble with the date and the addresses
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_deposting_addresses()
#' }
get_depositing_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                     api_key = Sys.getenv("GN_API_KEY"),
                                     as_date=TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  tmp_name <- glue::glue({{asset}},"_deposit_add")
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/addresses/sending_to_exchanges_count"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00"))
  if (as_date & (frequency %in% c("24h", "1w","1month"))) {
    x$date <- as.Date(x$date)
  }
  return(x)

}


#' Get the number of unique addresses that received funds
#'
#'
#' @param asset Asset to examine (see glassnode api docs for a list)
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h,10m,1w,1month
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#' @param as_date A logical to return a Date not a POSIXct object for daily or lower frequency
#'
#' @return return a tibble with the date and the addresses
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_receiving_addresses()
#' }
get_receiving_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                    api_key = Sys.getenv("GN_API_KEY"),
                                    as_date=TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  tmp_name <- glue::glue({{asset}},"_recv_add")
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/addresses/receiving_count"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00"))
  if (as_date & (frequency %in% c("24h", "1w","1month"))) {
    x$date <- as.Date(x$date)
  }
  return(x)

}

#' Get the number of addresses that sent tokens during the period
#'
#' @param asset Asset to examine (see glassnode api docs for a list)
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h,10m,1w,1month
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#' @param as_date A logical to return a Date not a POSIXct object for daily or lower frequency
#'
#' @return return a tibble with the date and the addresses
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_sending_addresses()
#' }
get_sending_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                  api_key = Sys.getenv("GN_API_KEY"),
                                  as_date=TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  tmp_name <- glue::glue({{asset}},"_sent_add")
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/addresses/sending_count"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00"))
  if (as_date & (frequency %in% c("24h", "1w","1month"))) {
    x$date <- as.Date(x$date)
  }
  return(x)

}

#' Get the number of unique addresses that was ever active
#'
#' @param asset Asset to examine (see glassnode api docs for a list)
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h,10m,1w,1month
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#' @param as_date A logical to return a Date not a POSIXct object for daily or lower frequency
#'
#' @return return a tibble with the date and the addresses
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_total_addresses()
#' }
get_total_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                api_key = Sys.getenv("GN_API_KEY"),
                                as_date=TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  tmp_name <- glue::glue({{asset}},"_tot_add")
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/addresses/count"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00"))
  if (as_date & (frequency %in% c("24h", "1w","1month"))) {
    x$date <- as.Date(x$date)
  }
  return(x)

}


#' Get the number of unique addresses that were active during the period
#'
#' @param asset Asset to examine (see glassnode api docs for a list)
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h,10m,1w,1month
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#' @param as_date A logical to return a Date not a POSIXct object for daily or lower frequency
#'
#' @return return a tibble with the date and the addresses
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_active_addresses()
#' }
get_active_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                 api_key = Sys.getenv("GN_API_KEY"),
                                 as_date=TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  tmp_name <- glue::glue({{asset}},"_active_add")
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/addresses/active_count"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00"))
  if (as_date & (frequency %in% c("24h", "1w","1month"))) {
    x$date <- as.Date(x$date)
  }
  return(x)

}
