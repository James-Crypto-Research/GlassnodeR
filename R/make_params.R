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
