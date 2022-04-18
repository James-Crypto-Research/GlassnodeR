#' Title
#'
#' @param api_key
#' @param ...
#'
#' @return
#'
#' @examples
make_params <- function(api_key=Sys.getenv("GN_API_KEY"),...){
    params <- list(...)
    params["f"] <- "json"
    params['timestamp_format'] <- "unix"
    params["api_key"] = api_key
    params <- plyr::compact(params)
    #In case there is no frequency make it 24h
    if (!("i" %in% names(params))) {
      params["i"] <- "24h"
    }
  return(params)
}
