t <- v <- NULL

#' Get the Total Value Locked for all DeFi platforms
#'
#' @param params A list of parameters to pass to the API
#' @param api_key The API key to use. Checks the env variable API_KEY
#'
#' @return
#' @export
#'
#' @examples

get_tvl <- function(since=NULL,until=NULL,api_key = Sys.getenv("API_KEY")){
    tmp <- list("asset" = "ETH",
                "frequency" = frequency,
                "api_key" = api_key,
                "since" = since,
                "until" = until)
    tmp["api_key"] <- api_key
    params <- do.call(make_params, tmp)
    x <- call_glassnode_api(
      path="v1/metrics/defi/total_value_locked",params
    ) |> tibble::as_tibble() |>
      dplyr::rename(date=t,tvl=v) |>
      dplyr::mutate(date=as.Date(as.POSIXct(date,origin="1970-01-01")))
    return(x)
  }
