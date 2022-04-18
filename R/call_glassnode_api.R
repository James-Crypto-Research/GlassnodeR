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
call_glassnode_api <- function(path, params) {
  tmp_url <- httr::modify_url("https://api.glassnode.com/", query=params,path = path)
  resp <- httr::GET(url = tmp_url)
  if (httr::http_error(resp)) {
    msg <- glue::glue(
      "Glassnode API request failed ({httr::status_code(resp)})","\n", url
    )
    stop(
      msg,
    )
  }
  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = TRUE)

  return(parsed)
}

