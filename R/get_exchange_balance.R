t <- asset <- NULL

#' Get Balances for Exchange Wallets
#'
#' @param params A list of parameters to pass to the API call
#' @param api_key The API key to use. By default it will check the API_KEY evn variable
#'
#' @return
#' @export
#' @importFrom rlang :=
#' @examples
get_exchange_balance <- function(asset="BTC",since=NULL,until=NULL,
                                 frequency="24h",
                                 exchange="aggregated",
                                 currency="USD",
                                 api_key = Sys.getenv("GN_API_KEY"),
                                 as_date=TRUE){
  tmp <- list("a" = asset,
                 "s" = since,
                 "u" = until,
                 "i" = frequency,
                 "e" = exchange,
                 "c" = currency,
                 "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/distribution/balance_exchanges"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{asset}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  x$exchange <- exchange
  return(x)
}
