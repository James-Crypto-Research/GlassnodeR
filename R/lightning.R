#' call the GN API for lightening parameters
#'
#' @param var_name Specific lightning network parameter
#' @param params list of parameters to query the API with
#'
#' @return a tibble with the datetime and the specific attribute
#' @importFrom rlang :=
#'
#' @examples
#'  \dontrun{
#' # Need a valid API to run
#' x <- call_lightning_api()
#' }
#' @noRd
call_lightning_api <- function(var_name,
                                params){

  return_val <- call_glassnode_api(
    path= glue::glue( "v1/metrics/lightning/", var_name),params
  ) |> tibble::as_tibble() |>
    dplyr::rename(date=t,{{var_name}}:= v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01", tz="UTC"))
  unames <- names(return_val)[2]
  return_val <- return_val |> tidyr::unnest(cols = unames)
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
#'  - Lightning network median base fee (fee_base_median)
#'  - Lightning netowrk fee rate (fee_rate_median)
#'  - Network Gini capacity distribution (gini_capacity)
#'  - Network Gini Channel Distribution (gini_channel)
#'  - Network node connectivity (node_connectivity)
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
#' x <- get_lightning_network()
#' }
get_lightning_network <- function(since=NULL, until=NULL, frequency = "24h",
                                  currency="USD",
                                  api_key = Sys.getenv("GN_API_KEY"),
                                  as_date=TRUE){
  paths = list(
    "channel_size_mean",
    "channel_size_median",
    "network_capacity_sum",
    "channels_count",
    "nodes_count",
    "base_fee_median",
    "fee_rate_median",
    "gini_capacity_distribution",
    "gini_channel_distribution",
    "node_connectivity"
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
  x <- dplyr::as_tibble(x)
  return(x)

}


#' Get Lightning Network Base Fee (Median)
#'
#' The median base fee in the Lightning Network in Satoshi.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and median base fee in Satoshi
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_lightning_base_fee()
#' }
get_lightning_base_fee <- function(since=NULL, until=NULL,
                                   frequency="24h",
                                   api_key = Sys.getenv("GN_API_KEY"),
                                   as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/lightning/base_fee_median"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, base_fee_median=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Lightning Network Total Capacity
#'
#' The total amount of BTC locked in the Lightning Network.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and network capacity in BTC
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_lightning_network_capacity()
#' }
get_lightning_network_capacity <- function(since=NULL, until=NULL,
                                           frequency="24h",
                                           api_key = Sys.getenv("GN_API_KEY"),
                                           as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/lightning/network_capacity_sum"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, network_capacity=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Lightning Network Channel Size (Mean)
#'
#' The mean BTC size of public Lightning Network channels.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and mean channel size in BTC
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_lightning_channel_size_mean()
#' }
get_lightning_channel_size_mean <- function(since=NULL, until=NULL,
                                            frequency="24h",
                                            api_key = Sys.getenv("GN_API_KEY"),
                                            as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/lightning/channel_size_mean"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, channel_size_mean=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Lightning Network Channel Size (Median)
#'
#' The median BTC size of public Lightning Network channels.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and median channel size in BTC
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_lightning_channel_size_median()
#' }
get_lightning_channel_size_median <- function(since=NULL, until=NULL,
                                              frequency="24h",
                                              api_key = Sys.getenv("GN_API_KEY"),
                                              as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/lightning/channel_size_median"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, channel_size_median=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Lightning Network Fee Rate (Median)
#'
#' The median fee rate in the Lightning Network.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and median fee rate
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_lightning_fee_rate()
#' }
get_lightning_fee_rate <- function(since=NULL, until=NULL,
                                   frequency="24h",
                                   api_key = Sys.getenv("GN_API_KEY"),
                                   as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/lightning/fee_rate_median"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, fee_rate_median=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Lightning Network Gini Capacity Distribution
#'
#' Statistical measure of Bitcoin capacity distribution among nodes.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and Gini capacity distribution
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_lightning_gini_capacity()
#' }
get_lightning_gini_capacity <- function(since=NULL, until=NULL,
                                        frequency="24h",
                                        api_key = Sys.getenv("GN_API_KEY"),
                                        as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/lightning/gini_capacity_distribution"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, gini_capacity_distribution=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Lightning Network Gini Channel Distribution
#'
#' Statistical measure of channel distribution among nodes.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and Gini channel distribution
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_lightning_gini_channels()
#' }
get_lightning_gini_channels <- function(since=NULL, until=NULL,
                                        frequency="24h",
                                        api_key = Sys.getenv("GN_API_KEY"),
                                        as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/lightning/gini_channel_distribution"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, gini_channel_distribution=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Lightning Network Node Connectivity
#'
#' Number of nodes connected via IP, TOR, or both.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and node connectivity metrics
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_lightning_node_connectivity()
#' }
get_lightning_node_connectivity <- function(since=NULL, until=NULL,
                                            frequency="24h",
                                            api_key = Sys.getenv("GN_API_KEY"),
                                            as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/lightning/node_connectivity"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, node_connectivity=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Lightning Network Channels Count
#'
#' Number of public Lightning Network channels.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and channels count
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_lightning_channels_count()
#' }
get_lightning_channels_count <- function(since=NULL, until=NULL,
                                         frequency="24h",
                                         api_key = Sys.getenv("GN_API_KEY"),
                                         as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/lightning/channels_count"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, channels_count=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Get Lightning Network Nodes Count
#'
#' Number of Lightning Network nodes.
#'
#' @param api_key The API key to use. By default it will check the GN_API_KEY environmental variable
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#' @param frequency A resolution for the data. See API documentation but it defaults to 24h
#' @param as_date A logical to return a date-time object or a date object for daily observations
#'
#' @return Returns a tibble with columns for the datetime and nodes count
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' #Need a valid API key to run
#' x <- get_lightning_nodes_count()
#' }
get_lightning_nodes_count <- function(since=NULL, until=NULL,
                                      frequency="24h",
                                      api_key = Sys.getenv("GN_API_KEY"),
                                      as_date=TRUE){
  tmp <- list("a" = "btc",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = "v1/metrics/lightning/nodes_count"),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, nodes_count=v) |>
    dplyr::mutate(date=as.POSIXct(date, origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & frequency == "24h"){
    x$date <- as.Date(x$date)
  }
  return(x)
}