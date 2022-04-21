#' Get the total rate of transactions per second in the given frequency
#'
#' @param asset The chain to examine. Limited to BTC, ETH, LTC
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h,10m,1w,1month
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#' @param as_date A logical to return a Date not a POSIXct object for daily or lower frequency
#'
#' @return
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
              "f" = frequency,
              "api_key" = api_key)
  tmp_name <- glue::glue({{asset}},"_tx_rate")
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/transactions/rate"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00"))
  if (as_date & (frequency %in% c("24h", "1w","1month"))) {
    x$date <- as.Date(x$date)
  }
  return(x)

}
