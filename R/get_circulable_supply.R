#'  Get the total amount of a coin that is availble at a given moment
#'
#' @param asset The asset whose supply to get (see list on Glassnode)
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param as_date A logical to return a date-time object or a date object for daily observations
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h

#' @return a tidy data frame of data
#' @export
#'
#' @examples
#' \dontrun{
#' # Need a valid API key to run
#' x <- get_circulable_supply()
#' }
get_circulable_supply <- function(asset="BTC",since=NULL,until=NULL,
                                 frequency="24h",
                                 api_key = Sys.getenv("GN_API_KEY"),
                                 as_date = TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/supply/current")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{asset}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)

}
