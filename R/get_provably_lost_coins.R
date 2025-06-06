#' Get the amount of coins that are considered lost or HODLed
#'
#' This is a calculated statistics
#'
#' @param asset Asset to examine (BTC or LTC)
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h
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
get_provably_lost_coins <- function(asset="BTC", since=NULL, until=NULL, frequency="24h",
                                   api_key = Sys.getenv("GN_API_KEY"),
                                   as_date=TRUE) {
  tmp_name <- glue::glue("{asset}_provably_lost")
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/supply/probably_lost"), 
    a = asset,
    s = since,
    u = until,
    i = frequency,
    api_key = api_key
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  
  if (as_date & (frequency %in% c("24h"))) {
    x$date <- as.Date(x$date)
  }
  return(x)
}
