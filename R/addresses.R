# This contains all the address functions
#'
#'#' Get all addresses statistics during the period
#'
#' @param asset Asset to examine (see glassnode api docs for a list)
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h,10m,1w,1month
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#' @param as_date A logical to return a Date not a POSIXct object for daily or lower frequency
#'
#' @return return a tibble with the date and the addresses
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_addresses()
#' }
get_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                          api_key = Sys.getenv("GN_API_KEY"),
                          as_date=TRUE) {

  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  # This is the list of end points and variable names
  end_points <- c("receiving_from_exchanges_count","sending_to_exchanges_count",
                  "receiving_count", "sending_count",
                  "active_count","new_non_zero_count",
                  "count")
  var_names <- c("with_add","dep_add",
                 "recv_add","sent_add",
                 "active_add","new_add",
                 "tot_add")
  x <- purrr::map2(var_names,end_points,call_address_api,params,as_date) |>
    plyr::join_all(by="date") |> tibble::as_tibble()
  return(x)
}

#' @rdname get_addresses
get_withdrawing_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                      api_key = Sys.getenv("GN_API_KEY"),
                                      as_date=TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- call_address_api("_with_add","receiving_from_exchanges_count", params,as_date)
  return(x)
}



#' @rdname get_addresses
get_new_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                              api_key = Sys.getenv("GN_API_KEY"),
                              as_date=TRUE) {
    tmp <- list("a" = asset,
                "s" = since,
                "u" = until,
                "i" = frequency,
                "api_key" = api_key)
    params <- do.call(make_params, tmp)
    x <- call_address_api("_new_add","new_non_zero_count", params,as_date)
    return(x)
}

#' @rdname get_addresses
get_depositing_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                     api_key = Sys.getenv("GN_API_KEY"),
                                     as_date=TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- call_address_api("_dep_add","sending_to_exchanges_count", params,as_date)
  return(x)
}


#' @rdname get_addresses
get_receiving_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                    api_key = Sys.getenv("GN_API_KEY"),
                                    as_date=TRUE) {
    tmp <- list("a" = asset,
                "s" = since,
                "u" = until,
                "i" = frequency,
                "api_key" = api_key)
    params <- do.call(make_params, tmp)
    x <- call_address_api("_recv_add","receiving_count", params,as_date)
    return(x)
}

#' @rdname get_addresses
get_sending_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                  api_key = Sys.getenv("GN_API_KEY"),
                                  as_date=TRUE) {
    tmp <- list("a" = asset,
                "s" = since,
                "u" = until,
                "i" = frequency,
                "api_key" = api_key)
    params <- do.call(make_params, tmp)
    x <- call_address_api("_sent_add","sending_count", params,as_date)
    return(x)
}

#' @rdname get_addresses
get_total_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                api_key = Sys.getenv("GN_API_KEY"),
                                as_date=TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- call_address_api("_tot_add","count",params, as_date)
  return(x)
}

#' @rdname get_addresses
get_active_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                 api_key = Sys.getenv("GN_API_KEY"),
                                 as_date=TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = frequency,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- call_address_api("_active_add","active_count",params, as_date)
  return(x)
}

call_address_api <- function(var_name,api_endpoint,params,as_date){
  tmp_name <- glue::glue({{params$a}},"_",var_name)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/addresses/",api_endpoint), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  if (as_date & (params$i %in% c("24h", "1w","1month"))) {
    x$date <- as.Date(x$date)
  }
  return(x)
}
