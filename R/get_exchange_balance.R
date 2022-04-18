t <- token <- NULL

#' Get Balances for Exchange Wallets
#'
#' @param params A list of parameters to pass to the API call
#' @param api_key The API key to use. By default it will check the API_KEY evn variable
#'
#' @return
#' @export
#' @importFrom rlang :=
#' @examples
get_exchange_balance <- function(asset,since,until,frequency="24",currency="USD",api_key = Sys.getenv("API_KEY")){
  params["api_key"] <-  api_key

  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/distribution/balance_exchanges"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{token}} :=v) |>
    dplyr::mutate(date=as.Date(as.POSIXct(date,origin="1970-01-01")))
  return(x)
}
