#' Get amount of a given asset is deposited in smart contracts
#'
#' This function returns the percentage (in decimal form) of all
#' circulable supply of the asset that is deposited in a smart contract
#'
#'
#' @param asset The asset to query. Default is ETH
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return a tidy data frame of data
#' @export
#'
#' @examples
#' \dontrun{
#' # Need a valid API key to run
#' x <- get_supply_contracts()
#' }
get_supply_contracts <- function(asset="ETH",since=NULL,until=NULL,
                                 api_key = Sys.getenv("GN_API_KEY")) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/distribution/supply_contracts"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{asset}} :=v) |>
    dplyr::mutate(date=as.Date(as.POSIXct(date,origin="1970-01-01 00:00:00")))
  return(x)

}
