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
