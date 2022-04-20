#' call the GN API for lightening parameters
#'
#' @param var_name Specific lightning network parameter
#' @param params list of parameters to query the API with
#'
#' @return a tibble with the datetime and the specific attribute
#' @importFrom rlang :=
#'
#' @examples
call_lightning_api <- function(var_name,
                                params){

  return_val <- call_glassnode_api(
    path= glue::glue( "v1/metrics/lightning/", var_name),params
  ) |> tibble::as_tibble() |>
    dplyr::rename(date=t,{{var_name}}:= v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01"))
  return(return_val)
}


#' Get statistics on the Lightning Network
#'
#' This function retrieves all available statistisc on the lightning network and returns them
#' as a tidy data set.
#'
#' The specific functions it returns are:
#'  - Mean channel size (channel_size_mean)
#'  - Median channel size (channel_size_median)
#'  - Total capacity of the network (network_capacity_sum)
#'  - The total number of channels (channels_count)
#'  - The total number of nodes (nodes_count)
#'  The default is to return the value amounts as USD unless "NATIVE" is chosen for currency
#'
#' @param api_key The api key used to retrieve the data
#' @param since,until A POSIX compatible value that will be converted to a unix data number
#' @param frequency What resolution to use. See API docs for possible values. The default is "24h" (daily)
#' @param currency What currency to use. Choices are "USD" or "NATIVE"
#' @param as_date A logical for turning the datatime object returned to a date object if it is daily data
#'
#' @return A tidy data set of characteristics
#' @export
#'
#' @examples
#' \dontrun{
#' # Need a valid API key to run
#' x <- get_lightning_network
#' }
get_lightning_network <- function(since=NULL, until=NULL, frequency = "24h", currency="USD", api_key = Sys.getenv("GN_API_KEY"),as_date=TRUE){
  paths = list(
    "channel_size_mean",
    "channel_size_median",
    "network_capacity_sum",
    "channels_count",
    "nodes_count"
  )
tmp <- list(
    "a" = "btc",
    "s" = since,
    "u" = until,
    "i" = frequency,
    "c" = currency,
    "api_key" = api_key
  )
  params <- do.call(make_params,tmp)
  x <- paths |> purrr::map(call_lightning_api, params) |>
    plyr::join_all(by="date")
  if (as_date & params["i"] == "24h"){
    x$date <- as.Date(x$date)
  }

  return(x)

}
