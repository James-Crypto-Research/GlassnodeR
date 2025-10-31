t <- asset <- NULL

#' Get the percent of circulable supply for a crypto that is at deposited in
#' large accoints
#'
#' This function calls top 1% balance call. It returns a tibble with the amount
#' that is the percent (in decimal form) of the crypto in wallets containing
#' the largest top 1% of balances. Smart contracts, exchanges and other identified
#' wallets are excluded.
#'
#' NOTE: FOr BTC this is entities (cluster of wallets) for others this is wallets
#'
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime, and percent
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_percent_top_balance()
#' }
get_percent_top_balance <- function(asset="BTC",since=NULL,until=NULL,
                                     frequency="24h",
                                     api_key = Sys.getenv("GN_API_KEY"),
                                     as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  var_name <- glue::glue({{asset}},"_pct")
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/distribution/balance_1pct_holders")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{var_name}} := v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}
