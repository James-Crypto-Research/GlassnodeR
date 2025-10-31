
# This contains the functions to grab miner data


#' Get revenue (fee and block reward) for miners
#'
#' These functions grab the revenue in USD of blocks of miners. The revenue
#' comes from fees and from block rewards. The function get_revenue_miners
#' returns either jsut the aggregated (if get_agg is true) or all miner
#' revenues. Individual miners revenue can be found using get_miner_rev and
#' passing the name of the miner.
#'
#' NOTE: Aggregated miner revenue is available for ETH and BTC but individual
#' miner revenue is only available for BTC.
#'
#'
#'
#' @param chain The chain to use ("BTC" or "ETH")
#' @param since The date to start the extraction
#' @param until The date to end the extraction
#' @param frequency What frequency to pull (either "1h" or "24h")
#' @param get_agg A logical for whether to get all miners
#' @param currency Should the value be returned in "USD" or "NATIVE"
#' @param api_key The API key. It defaults to the GN_API_KEY environmental variables
#' @param as_date A logical whether to return a date object if the frequency is "24h"
#' @param miner The name of the miner to use
#'
#' @return This returns a
#' @export
#'
get_revenue_miners <- function(chain="BTC",
                               since=NULL,until=NULL,
                                        frequency="24h",
                                        get_agg=TRUE,
                                        currency="USD",
                                        api_key = Sys.getenv("GN_API_KEY"),
                                        as_date=TRUE){
  # Note: The /miners endpoint appears to be unavailable in the current API
  # For now, we only support aggregated miner data
  if (get_agg) {
    # User wants only aggregated data
    miner_list <- "aggregated"
  } else {
    # User wants all miners - but the /miners endpoint returns 404
    # So we fall back to just aggregated for now
    warning("Individual miner data endpoint is currently unavailable. Returning aggregated data only.")
    miner_list <- "aggregated"
  }
  x <- purrr::map(miner_list,get_miner_rev,chain=chain,since=since,until=until,
                frequency=frequency,currency=currency,api_key=api_key,
                as_date=as_date) |> plyr::join_all(by="date") |>
    tibble::as_tibble()
  if (as_date & (frequency == "24h")) {
    x$date <- as.Date(x$date)
  }
  return(x)
}


#' @rdname get_revenue_miners
get_miner_rev <- function(miner="aggregated",
                          chain="BTC",
                          since=NULL,
                          until=NULL,
                          frequency="24h",
                          currency="USD",
                          api_key = Sys.getenv("GN_API_KEY"),
                          as_date=TRUE){

  tmp <- list("a" = chain,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "c" = currency,
              "miner" = miner,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/mining/revenue_sum")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{miner}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & (frequency == "24h")) {
    x$date <- as.Date(x$date)
  }
  return(x)
}



#' Get volume mined  for BTC miners
#'
#' These functions grab the number of coins  miners have won.
#'  The function get_vol_miners
#' returns either jsut the aggregated (if get_agg is true) or all miner
#' volumes. Individual miner's volumescan be found using get_miner_vol and
#' passing the name of the miner.
#'
#' NOTE: These amounts are available only for BTC.
#'
#'
#' @param since The date to start the extraction
#' @param until The date to end the extraction
#' @param frequency What frequency to pull (either "1h" or "24h")
#' @param get_agg A logical for whether to get all miners
#' @param api_key The API key. It defaults to the GN_API_KEY environmental variables
#' @param as_date A logical whether to return a date object if the frequency is "24h"
#' @param miner The name of the miner to use
#'
get_vol_miners <- function(since=NULL,until=NULL,
                               frequency="24h",
                               get_agg=TRUE,
                               api_key = Sys.getenv("GN_API_KEY"),
                               as_date=TRUE){
  # Note: The /miners endpoint appears to be unavailable in the current API
  # For now, we only support aggregated miner data
  if (get_agg) {
    # User wants only aggregated data
    miner_list <- "aggregated"
  } else {
    # User wants all miners - but the /miners endpoint returns 404
    # So we fall back to just aggregated for now
    warning("Individual miner data endpoint is currently unavailable. Returning aggregated data only.")
    miner_list <- "aggregated"
  }
  x <- purrr::map(miner_list,get_miner_vol,since=since,until=until,
                       frequency=frequency,api_key=api_key,
                       as_date=as_date) |> plyr::join_all(by="date") |>
    tibble::as_tibble()
  if (as_date & (frequency == "24h")) {
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' @rdname get_vol_miners
get_miner_vol <- function(miner="aggregated",
                          since=NULL,
                          until=NULL,
                          frequency="24h",
                          api_key = Sys.getenv("GN_API_KEY"),
                          as_date=TRUE){

  tmp <- list("a" = "BTC",
              "s" = since,
              "u" = until,
              "i" = frequency,
              "miner" = miner,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- do.call(call_glassnode_api, c(
    list(path = glue::glue("v1/metrics/mining/volume_mined_sum")),
    params
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{miner}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & (frequency == "24h")) {
    x$date <- as.Date(x$date)
  }
  return(x)
}
