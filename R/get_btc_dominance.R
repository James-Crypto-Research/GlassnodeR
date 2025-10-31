#' Get BTC Dominance
#'
#' BTC Dominance represents the percentage of the total cryptocurrency market cap
#' that is represented by Bitcoin. It's a key metric for understanding Bitcoin's
#' relative position in the crypto market.
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and BTC dominance percentage
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_btc_dominance()
#' }
get_btc_dominance <- function(since=NULL,until=NULL,
                              frequency="24h",
                              api_key = Sys.getenv("GN_API_KEY"),
                              as_date=TRUE){
  tmp <- list("s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/market/btc_dominance")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, btc_dominance=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}