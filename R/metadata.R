# Metadata and catalog discovery functions

#' Get Supported Assets from Glassnode
#'
#' Queries the Glassnode metadata catalog for supported digital assets. Supports
#' Common Expression Language (CEL) filtering via the \code{filter} argument
#' (for example, \code{"asset.semantic_tags.exists(tag,tag=='stablecoin')"}).
#'
#' @param filter Optional CEL filter expression to narrow returned assets
#' @param api_key The API key to use. By default it checks the GN_API_KEY
#'   environment variable
#'
#' @return A tibble of supported assets and their metadata
#' @export
#'
#' @examples
#' \dontrun{
#' # All supported assets
#' assets <- get_assets()
#'
#' # Only stablecoins
#' stables <- get_assets(filter = "asset.semantic_tags.exists(tag,tag=='stablecoin')")
#' }
get_assets <- function(filter = NULL, api_key = Sys.getenv("GN_API_KEY")) {
  query <- list(path = "v1/metadata/assets", api_key = api_key)
  if (!is.null(filter) && nzchar(filter)) {
    query$filter <- filter
  }
  do.call(call_glassnode_api, query) |>
    tibble::as_tibble()
}

#' Get Tracked Stablecoin Assets
#'
#' Convenience wrapper around \code{\link{get_assets}} that filters the
#' Glassnode asset catalog to assets tagged as \code{"stablecoin"}.
#'
#' @param api_key The API key to use. By default it checks the GN_API_KEY
#'   environment variable
#'
#' @return A tibble of stablecoin assets supported by Glassnode
#' @export
#'
#' @examples
#' \dontrun{
#' stables <- get_stablecoin_assets()
#' }
get_stablecoin_assets <- function(api_key = Sys.getenv("GN_API_KEY")) {
  get_assets(
    filter = "asset.semantic_tags.exists(tag,tag=='stablecoin')",
    api_key = api_key
  )
}

#' List Available Glassnode Metric Endpoints
#'
#' Queries \code{/v1/metadata/metrics} to list available metric paths, optionally
#' filtered by asset, exchange, or resolution.
#'
#' @param asset Optional asset ticker to filter metrics (e.g. \code{"USDT"},
#'   \code{"USDC"}, \code{"BTC"})
#' @param exchange Optional exchange identifier to filter metrics (e.g.
#'   \code{"binance"})
#' @param frequency Optional resolution interval to filter metrics (e.g.
#'   \code{"24h"}, \code{"1h"})
#' @param api_key The API key to use. By default it checks the GN_API_KEY
#'   environment variable
#'
#' @return A tibble with columns \code{path}, \code{category}, and \code{metric}
#' @export
#'
#' @examples
#' \dontrun{
#' usdt_metrics <- get_metrics(asset = "USDT")
#' }
get_metrics <- function(asset = NULL,
                        exchange = NULL,
                        frequency = NULL,
                        api_key = Sys.getenv("GN_API_KEY")) {
  query <- purrr::compact(list(
    path = "v1/metadata/metrics",
    a = asset,
    e = exchange,
    i = frequency,
    api_key = api_key
  ))
  paths <- as.character(do.call(call_glassnode_api, query))
  cleaned <- sub("^/", "", paths)
  parts <- strsplit(cleaned, "/", fixed = TRUE)
  tibble::tibble(
    path = paths,
    category = vapply(parts, function(p) if (length(p) >= 1L) p[[1L]] else NA_character_, character(1L)),
    metric = vapply(parts, function(p) if (length(p) >= 2L) paste(p[-1L], collapse = "/") else NA_character_, character(1L))
  )
}

#' Get Metadata for a Specific Glassnode Metric
#'
#' Queries \code{/v1/metadata/metric} for detailed parameter metadata (supported
#' assets, resolutions, currencies, exchanges) for a single metric path.
#'
#' @param path Metric path as returned by \code{\link{get_metrics}} (e.g.
#'   \code{"/market/marketcap_stablecoins_usd"} or \code{"/indicators/velocity"})
#' @param asset Optional asset ticker to scope parameter metadata
#' @param api_key The API key to use. By default it checks the GN_API_KEY
#'   environment variable
#'
#' @return A list containing the parsed metric metadata
#' @export
#'
#' @examples
#' \dontrun{
#' meta <- get_metric_metadata("/transactions/transfers_volume_sum", asset = "USDC")
#' }
get_metric_metadata <- function(path,
                                asset = NULL,
                                api_key = Sys.getenv("GN_API_KEY")) {
  if (missing(path) || !is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be a non-empty character string")
  }
  # Glassnode expects the metric path in the `path` query parameter,
  # while call_glassnode_api uses `path` for the URL path.
  full_url <- httr::modify_url(
    "https://api.glassnode.com/v1/",
    path = "v1/metadata/metric",
    query = purrr::compact(list(path = path, a = asset))
  )
  safe_url <- redact_url(full_url)
  key <- if (is.character(api_key) && length(api_key) == 1L && nzchar(api_key)) {
    api_key
  } else {
    get_default_api_key()
  }
  headers <- if (nzchar(key)) httr::add_headers("X-Api-Key" = key) else httr::add_headers()
  resp <- httr::GET(full_url, headers, httr::timeout(30))
  if (httr::http_error(resp)) {
    stop(
      sprintf("Glassnode API request failed (Status: %s)\nURL: %s", httr::status_code(resp), safe_url),
      call. = FALSE
    )
  }
  httr::content(resp, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()
}
