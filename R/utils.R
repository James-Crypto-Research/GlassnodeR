# This file contains the utility functions

#' Call Glassnode API
#'
#'  This is the core function connecting to Glassnode. Since all (most?) API
#'  calls have the same seven parameters it does some error checking
#'  and adds defaults all the
#'
#' @param path The path to pass to the API URL
#' @param params a set of parameters to pass the API.
#'
#' @return a parsed list from the JSON structure
#'
#' @examples
#' \dontrun{
#' # Need a valid API to run
#' x <- call_glassnode_api()
#' }
#' @noRd
call_glassnode_api <- function(path, ...) {
  tmp <- list(...)
  params <- do.call(make_params, tmp)
  tmp_url <- httr::modify_url("https://api.glassnode.com/", query=params,path = path)
  resp <- httr::GET(url = tmp_url)
  if (httr::http_error(resp)) {
    msg <- glue::glue(
      "Glassnode API request failed ({httr::status_code(resp)})","\n", tmp_url
    )
    stop(
      msg
    )
  }
  parsed <- httr::content(resp, "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()
  return(parsed)
}




#' Turn dates to unix datetime string
#'
#' This function is a utility function for make_params to make a number to
#' work in the API
#'
#' @param the_date a POSIXt or string that can be converted to it
#'
#' @return a unix numeric for datetime
#'
#' @examples
#' x <- sanitize_date("2020-01-01")
#' @noRd
sanitize_date <- function(the_date){
  # First check if it is a date, POSIXlt or string and if so convert it
  if (lubridate::is.Date(the_date)){
    the_date <- as.POSIXct(the_date)
  }
  if (is.character(the_date)){
    the_date <- as.POSIXct(the_date)
  }
  if (lubridate::is.POSIXlt(the_date)){
    the_date <- as.POSIXct(the_date)
  }
  if (lubridate::is.POSIXct(the_date)){
    the_date <- as.numeric(the_date)
  } else{
    stop("Not a POSIXct object or can't be coerced to it")
  }
  return(the_date)
}


#' Regularize parameters into the Glassnode API
#'
#' This function makes sure some of the basic parameters are there for
#' the Glassnode API. Also ensures a return format that is known. Also defaults
#' the frequency to daily unless overridden
#'
#' TODO: add more error checking to this function
#'
#' @param api_key The API key to use. By default it will check the API_KEY environmental variable
#' @param ... A list of parameters for use
#'
#' @return A list of parameters ready to send to the GN API
#'
#' @examples
#' params <- list(a="BTC",s="2020-01-01",u="2021-01-01")
#' x <- make_params(params)
#' @noRd
make_params <- function(api_key=Sys.getenv("GN_API_KEY"),...){
  params <- list(...)
  params["f"] <- "json"
  params['timestamp_format'] <- "unix"
  params["api_key"] = api_key
  params <- plyr::compact(params)
  if ("s" %in% names(params)){
    params["s"] <- sanitize_date(params[["s"]])
  }
  if ("u" %in% names(params)){
    params["u"] <- sanitize_date(params[["u"]])
  }

  #In case there is no frequency make it 24h
  if (!("i" %in% names(params))) {
    params["i"] <- "24h"
  }
  return(params)
}
