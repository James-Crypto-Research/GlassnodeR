# This file contains the utility functions

utils::globalVariables(c(
  "amount", "balance", "changes", "deposits", "duration",
  "exchange_fee_dominance", "exchange_fees", "exchange_fees_mean",
  "fees_distribution", "h", "l", "net_volume", "o", "price", "provably_lost",
  "realized_volatility", "supply", "t", "treasury_balance_companies",
  "treasury_balance_governments", "treasury_balance_relative_companies",
  "treasury_balance_relative_governments", "treasury_flows_net_companies",
  "treasury_flows_net_governments", "tvl_by_chain", "tvl_relative",
  "txs_count_distribution", "txs_size_distribution", "v", "value",
  "value_distribution", "withdrawals"
))

#' Resolve default Glassnode API key from environment variables
#'
#' Checks `GN_API_KEY` first, falling back to `GLASSNODE_API_KEY`.
#'
#' @return A character string (empty string if neither variable is set)
#' @noRd
get_default_api_key <- function() {
  key <- Sys.getenv("GN_API_KEY")
  if (!nzchar(key)) {
    key <- Sys.getenv("GLASSNODE_API_KEY")
  }
  key
}

#' Redact any api_key query parameter from a URL for safe logging
#'
#' @param url Character string URL
#' @return URL with api_key value redacted
#' @noRd
redact_url <- function(url) {
  gsub("([?&]api_key=)[^&]*", "\\1<REDACTED>", url)
}

#' Call Glassnode API
#'
#' This is the core function connecting to Glassnode. It sends the API key via
#' the `X-Api-Key` header, redacts any credentials from error messages, applies
#' a request timeout, and retries transient rate-limit (HTTP 429) responses.
#'
#' @param path The path to pass to the API URL
#' @param ... Query parameters to pass to the API
#' @param api_key Glassnode API key (defaults to `GN_API_KEY` or `GLASSNODE_API_KEY`)
#' @param timeout Request timeout in seconds (default 30)
#' @param max_retries Maximum number of retries on HTTP 429/5xx responses (default 3)
#'
#' @return A parsed list or data.frame from the JSON response
#'
#' @examples
#' \dontrun{
#' # Need a valid API key to run
#' x <- call_glassnode_api("v1/metrics/market/price_usd_close", a = "BTC")
#' }
#' @noRd
call_glassnode_api <- function(path, ...,
                               api_key = get_default_api_key(),
                               timeout = 30,
                               max_retries = 3) {
  # Validate inputs
  if (missing(path) || !is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("API path must be a non-empty character string")
  }

  # Prepare query parameters (keep api_key out of query string)
  query_params <- list(...)
  if ("api_key" %in% names(query_params)) {
    if (!nzchar(api_key) && is.character(query_params$api_key) && nzchar(query_params$api_key)) {
      api_key <- query_params$api_key
    }
    query_params$api_key <- NULL
  }

  base_url <- "https://api.glassnode.com/v1/"
  full_url <- httr::modify_url(
    base_url,
    path = path,
    query = query_params
  )
  safe_url <- redact_url(full_url)

  headers <- if (is.character(api_key) && length(api_key) == 1L && nzchar(api_key)) {
    httr::add_headers("X-Api-Key" = api_key)
  } else {
    httr::add_headers()
  }

  # Make API request with retry on rate limits / transient server errors
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    resp <- httr::GET(full_url, headers, httr::timeout(timeout))
    status <- httr::status_code(resp)

    if ((status == 429L || (status >= 500L && status < 600L)) && attempt <= max_retries) {
      retry_after <- suppressWarnings(as.numeric(httr::headers(resp)[["retry-after"]]))
      wait_secs <- if (!is.na(retry_after) && retry_after > 0) {
        retry_after
      } else {
        2^(attempt - 1L)
      }
      Sys.sleep(wait_secs)
      next
    }

    if (httr::http_error(resp)) {
      if (status %in% c(401L, 403L)) {
        stop(
          sprintf(
            "Glassnode API authentication/authorization failed (Status: %s). Check GN_API_KEY.\nURL: %s",
            status,
            safe_url
          ),
          call. = FALSE
        )
      }
      stop(
        sprintf(
          "Glassnode API request failed (Status: %s)\nURL: %s",
          status,
          safe_url
        ),
        call. = FALSE
      )
    }

    parsed_content <- httr::content(resp, as = "text", encoding = "UTF-8") |>
      jsonlite::fromJSON()

    return(parsed_content)
  }
}

#' Turn dates to unix datetime string
#'
#' This function is a utility function for make_params to make a number to
#' work in the API
#'
#' @param the_date a POSIXt, Date, or string that can be converted to POSIXct
#'
#' @return a unix numeric for datetime
#'
#' @examples
#' x <- sanitize_date("2020-01-01")
#' @noRd
sanitize_date <- function(the_date) {
  tryCatch({
    if (!lubridate::is.POSIXct(the_date)) {
      the_date <- as.POSIXct(the_date, tz = "UTC")
    }
    if (is.na(the_date)) {
      stop("Converted date is NA")
    }
    as.numeric(the_date)
  },
  error = function(e) {
    stop("Unable to convert input to a valid date: ", e$message, call. = FALSE)
  })
}

#' Prepare parameters for Glassnode API
#'
#' Sanitizes and standardizes parameters for Glassnode API calls
#'
#' @param api_key API key (defaults to `GN_API_KEY` or `GLASSNODE_API_KEY`)
#' @param ... Additional parameters for the API call
#'
#' @return A named list of sanitized API parameters
#'
#' @importFrom rlang dots_list
#' @importFrom purrr compact
#' @importFrom lubridate as_datetime
make_params <- function(api_key = get_default_api_key(), ...) {
  # Collect all input parameters
  params <- rlang::dots_list(
    ...,
    f = "json",
    timestamp_format = "unix",
    api_key = api_key,
    .ignore_empty = "all"
  ) |>
    purrr::compact()

  # Add default frequency if not specified
  if (!"i" %in% names(params)) {
    params$i <- "24h"
  }

  # Sanitize date parameters
  date_params <- c("s", "u")
  for (param in date_params) {
    if (!is.null(params[[param]])) {
      params[[param]] <- tryCatch(
        sanitize_date(params[[param]]),
        error = function(e) {
          warning(glue::glue("Could not sanitize {param} parameter: {e$message}"))
          params[[param]]
        }
      )
    }
  }

  params
}
