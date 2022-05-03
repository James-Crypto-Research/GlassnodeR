t <- v <- NULL

#' Get the Total Value Locked for all DeFi platforms
#'
#' @param api_key The API key to use. Checks the env variable API_KEY
#' @param since,until A POSIX compatible date-time object. It's converted to a unix date number
#'
#' @return Returns a tidy data set denominated in USD
#' @export
#'
#' @examples
#' \dontrun{
#' # Need a API key to run
#' x <- get_tvl()
#' }

get_tvl <- function(since=NULL,until=NULL,api_key = Sys.getenv("GN_API_KEY")){
    tmp <- list("a" = "eth",
                "api_key" = api_key,
                "s" = since,
                "u" = until)
    params <- do.call(make_params, tmp)
    x <- call_glassnode_api(
      path="v1/metrics/defi/total_value_locked",params
    ) |> tibble::as_tibble() |>
      dplyr::rename(date=t,tvl=v) |>
      dplyr::mutate(date=as.Date(as.POSIXct(date,origin="1970-01-01", tz="UTC")))
    return(x)
  }
