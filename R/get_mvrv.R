#' Get the MVRV (Market Value to Realized Value) Ratio
#'
#' The MVRV ratio represents the ratio between market cap and realized cap.
#' It provides insight into whether an asset is over or undervalued relative to its "fair value".
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param asset This is the asset to get information on. The list of available assets is on the GN API site
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and MVRV ratio
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_mvrv()
#' }
get_mvrv <- function(asset="BTC",since=NULL,until=NULL,
                     frequency="24h",
                     api_key = Sys.getenv("GN_API_KEY"),
                     as_date=TRUE){
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/market/mvrv"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, mvrv=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}