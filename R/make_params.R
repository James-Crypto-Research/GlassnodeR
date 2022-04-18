#' Title
#'
#' @param asset
#' @param since
#' @param until
#' @param frequency
#' @param currency
#' @param api_key
#'
#' @return
#' @export
#'
#' @examples
make_params <- function(...){
    params <- list(...)
    params["format"] = "JSON"
    params["timestamp_format" = "unix"]
    params["api_key" = api_key]
    params <- compact(params)
  return(params)
}
