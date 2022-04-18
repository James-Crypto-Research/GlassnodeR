#' Title
#'
#' @param asset
#' @param since
#' @param until
#' @param api_key
#' @param as_date
#'
#' @return
#' @export
#'
#' @examples
get_supply_contracts <- function(asset="ETH",since=NULL,until=NULL,
                                 api_key = Sys.getenv("GN_API_KEY"),
                                 as_date=TRUE) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "api_key" = api_key)
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/distribution/supply_contracts"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{asset}} :=v) |>
    dplyr::mutate(date=as.Date(as.POSIXct(date,origin="1970-01-01 00:00:00")))
  return(x)

}
