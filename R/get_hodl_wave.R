#' Return the HODL waves for BTC or ETH
#'
#' @param asset
#' @param since
#' @param until
#' @param api_key
#'
#' @return a Tibble with the waves as columns
#' @export
#'
#' @examples
#' x <- get_hodl_wave()
get_hodl_wave <- function(asset="BTC",since=NULL,until=NULL,
                                  api_key = Sys.getenv("GN_API_KEY")) {
  tmp <- list("a" = asset,
              "s" = since,
              "u" = until,
              "i" = "24h",
              "api_key" = api_key)
  tmp_name <- glue::glue({{asset}},"_tx_rate")
  params <- do.call(make_params, tmp)
  x <- call_glassnode_api(
    path = glue::glue("v1/metrics/supply/hodl_waves"), params
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(date=t, {{tmp_name}} :=v) |>
    dplyr::mutate(date=as.POSIXct(date,origin="1970-01-01 00:00:00", tz="UTC"))
  y <- x$o
  y$date <- as.Date(x$date)
  x <- y |> relocate(date) |>
    pivot_longer(-date,names_to="duration", values_to="amount") |>
    mutate(amount = replace_na(amount, 0)) |>
    mutate(duration = as_factor(duration)) |>
    mutate(duration = fct_relevel(duration,
                                  "more_10y","7y_10y","5y_7y",
                                  "3y_5y","2y_3y","1y_2y",
                                  "6m_12m","3m_6m","1m_3m",
                                  "1w_1m","1d_1w","24h"))
  return(x)

}
