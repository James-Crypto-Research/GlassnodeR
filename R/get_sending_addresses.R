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
