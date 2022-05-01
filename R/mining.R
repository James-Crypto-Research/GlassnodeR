
# This contains the functions to grab miner data


#' Title
#'
#' @param chain
#' @param since
#' @param until
#' @param frequency
#' @param get_agg
#' @param currency
#' @param api_key
#' @param as_date
#'
#' @return
#' @export
#'
#' @examples
get_revenue_miners <- function(chain="BTC",
                               since=NULL,until=NULL,
                                        frequency="24h",
                                        get_agg=TRUE,
                                        currency="USD",
                                        api_key = Sys.getenv("GN_API_KEY"),
                                        as_date=TRUE){
  tmp <- list("api_key" = api_key)
  miner_params <- do.call(make_params, tmp)
  if ((!get_agg) | (chain != "ETH")){
      miner_list <- call_glassnode_api(
        path = glue::glue("/v1/metrics/mining/revenue_sum/miners"), miner_params
      )
    miner_list <- unlist(miner_list[[chain]])
  } else {
    miner_list <-  "aggregated"
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


#' Title
#'
#' @param miner
#' @param chain
#' @param since
#' @param until
#' @param frequency
#' @param currency
#' @param api_key
#' @param as_date
#'
#' @return
#' @export
#'
#' @examples
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
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/mining/revenue_sum"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{miner}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00"))
  if (as_date & (frequency == "24h")) {
    x$date <- as.Date(x$date)
  }
  return(x)
}



#' Title
#'
#' @param since
#' @param until
#' @param frequency
#' @param get_agg
#' @param api_key
#' @param as_date
#'
#' @return
#' @export
#'
#' @examples
get_vol_miners <- function(since=NULL,until=NULL,
                               frequency="24h",
                               get_agg=TRUE,
                               api_key = Sys.getenv("GN_API_KEY"),
                               as_date=TRUE){
  tmp <- list("api_key" = api_key)
  miner_params <- do.call(make_params, tmp)
  if ((!get_agg)){
    miner_list <- call_glassnode_api(
      path = glue::glue("/v1/metrics/mining/volume_mined_sum/miners"), miner_params
    )
    miner_list <- unlist(miner_list)
  } else {
    miner_list <-  "aggregated"
  }
  x <- purrr::map(miner_list,get_miner_rev,since=since,until=until,
                       frequency=frequency,api_key=api_key,
                       as_date=as_date) |> plyr::join_all(by="date") |>
    tibble::as_tibble()
  if (as_date & (frequency == "24h")) {
    x$date <- as.Date(x$date)
  }
  return(x)
}

#' Title
#'
#' @param miner
#' @param since
#' @param until
#' @param frequency
#' @param api_key
#' @param as_date
#'
#' @return
#' @export
#'
#' @examples
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
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/mining/volume_mined_sum"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{miner}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00"))
  if (as_date & (frequency == "24h")) {
    x$date <- as.Date(x$date)
  }
  return(x)
}
