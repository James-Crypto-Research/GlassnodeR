#' Get Bridge Deposits by Chain
#'
#' Retrieves bridge deposit data for specified blockchain assets
#'
#' @param asset Asset to examine (default: "ETH")
#' @param since Optional start date for data retrieval
#' @param until Optional end date for data retrieval
#' @param frequency Frequency of data points (default: "24h")
#' @param api_key API key for Glassnode (defaults to environment variable)
#' @param as_date Logical to return Date instead of POSIXct for daily frequency
#'
#' @return A tibble with date and bridge deposit data
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_bridge_deposits("ETH")
#' }
get_bridge_deposits <- function(asset = "ETH",
                                since = NULL,
                                until = NULL,
                                frequency = "24h",
                                api_key = Sys.getenv("GN_API_KEY"),
                                as_date = TRUE) {

  # Validate input asset
  if (!is.character(asset) || nchar(asset) == 0) {
    stop("Asset must be a non-empty character string")
  }

  # Prepare parameters
  params <- make_params(
    a = asset,
    s = since,
    u = until,
    i = frequency,
    api_key = api_key
  )

  # Create dynamic column name
  tmp_name <- glue::glue("{asset}_bridge_deposits")

  # Call API with dynamic parameters
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/bridges/deposits_by_chain"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date = t, {{tmp_name}} := v) |>
    dplyr::mutate(date = as.POSIXct(date, origin = "1970-01-01 00:00:00", tz = "UTC"))

  # Convert to Date if specified
  if (as_date && (frequency %in% c("24h"))) {
    x$date <- as.Date(x$date)
  }

  return(x)
}