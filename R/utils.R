# This file contains the utility functions

#' Call Glassnode API
#'
#'  This is the core function connecting to Glassnode. Since all (most?) API
#'  calls have the same seven parameters it does some error checking
#'  and adds defaults all the
#'
#' @param path The path to pass to the API URL
#' @param params a set of parameters to pass the API.
#'
#' @return a parsed list from the JSON structure
#'
#' @examples
#' \dontrun{
#' # Need a valid API to run
#' x <- call_glassnode_api()
#' }
#' @noRd
call_glassnode_api <- function(path, ...,
                               api_key = Sys.getenv("GLASSNODE_API_KEY")) {
  # Validate inputs
  if (missing(path)) {
    stop("API path must be provided")
  }
  
  # Prepare query parameters
  query_params <- list(...)
  query_params$api_key <- api_key
  
  base_url = "https://api.glassnode.com/v1/"
  # Construct full URL
  full_url <- httr::modify_url(
    base_url, 
    path = path, 
    query = query_params
  )
  
  # Make API request with error handling
  tryCatch({
    resp <- httr::GET(full_url)
    
    # Check for HTTP errors
    if (httr::http_error(resp)) {
      stop(
        sprintf(
          "Glassnode API request failed (Status: %s)\nURL: %s", 
          httr::status_code(resp), 
          full_url
        )
      )
    }
    
    # Parse response
    parsed_content <- httr::content(resp, as = "text", encoding = "UTF-8") |>
      jsonlite::fromJSON()
    
    return(parsed_content)
  }, 
  error = function(e) {
    # Enhanced error handling
    if (grepl("API key", e$message, ignore.case = TRUE)) {
      stop("Invalid or missing Glassnode API key. Please set GLASSNODE_API_KEY.")
    }
    stop(e)
  })
}

#' Turn dates to unix datetime string
#'
#' This function is a utility function for make_params to make a number to
#' work in the API
#'
#' @param the_date a POSIXt or string that can be converted to it
#'
#' @return a unix numeric for datetime
#'
#' @examples
#' x <- sanitize_date("2020-01-01")
#' @noRd
sanitize_date <- function(the_date) {
  # Use tryCatch to handle potential conversion errors
  tryCatch({
    # Use lubridate or base R functions for efficient conversion
    if (!lubridate::is.POSIXct(the_date)) {
      the_date <- as.POSIXct(the_date)
    }
    
    # Convert directly to numeric timestamp
    as.numeric(the_date)
  }, 
  error = function(e) {
    stop("Unable to convert input to a valid date: ", e$message)
  })
}

#' Prepare parameters for Glassnode API
#'
#' Sanitizes and standardizes parameters for Glassnode API calls
#'
#' @param api_key API key (defaults to environment variable)
#' @param ... Additional parameters for the API call
#'
#' @return A named list of sanitized API parameters
#'
#' @importFrom rlang dots_list
#' @importFrom purrr compact
#' @importFrom lubridate as_datetime
make_params <- function(api_key = Sys.getenv("GN_API_KEY"), ...) {
  # Collect all input parameters
  params <- rlang::dots_list(
    ...,
    f = "json",
    timestamp_format = "unix",
    api_key = api_key,
    .ignore_empty = "all"
  )
  
  # Remove NULL values and standardize
  params <- params %>%
    purrr::compact() %>%
    # Ensure consistent parameter handling
    tibble::as_tibble() %>%
    # Add default frequency if not specified
    {
      if (!"i" %in% names(.)) 
        dplyr::mutate(., i = "24h")
      else 
        .
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
  
  # Convert back to list and return
  as.list(params)
}
