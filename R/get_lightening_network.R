#' Title
#'
#' @param var_name
#' @param params
#' @param api_key
#'
#' @return
#' @importFrom rlang :=
#'
#' @examples
call_lightening_api <- function(var_name,
                                params){

  return_val <- call_glassnode_api(
    path= glue::glue( "v1/metrics/lightning/", var_name),params
  ) |> tibble::as_tibble() |>
    dplyr::rename(date=t,{{var_name}}:= v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01"))
  return(return_val)
}


#' Title
#'
#' @param params
#' @param api_key
#'
#' @return
#' @export
#'
#' @examples
get_lightening_network <- function(api_key = Sys.getenv("API_KEY"),...){
  paths = list(
    "channel_size_mean",
    "channel_size_median",
    "network_capacity_sum",
    "channels_count",
    "nodes_count"
  )
  params <- make_params(api_key,...)
  x <- paths |> purrr::map(call_lightening_api,
                         api_key=api_key,...) |>
    plyr::join_all(by="date")

  return(x)

}
