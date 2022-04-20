#' Turn dates to unix datetime string
#'
#' This function is a utility function for make_params to make a number to
#' work in the API
#'
#' @param the_date a POSIXt or string that can be converted to it
#'
#' @return a unix numeric for datetime
#' @export
#'
#' @examples
#' x <- sanitize_date("2020-01-01")
sanitize_date <- function(the_date){
  # First check if it is a date, POSIXlt or string and if so convert it
  if (lubridate::is.Date(the_date)){
    the_date <- as.POSIXct(the_date)
  }
  if (is.character(the_date)){
    the_date <- as.POSIXct(the_date)
  }
  if (lubridate::is.POSIXlt(the_date)){
    the_date <- as.POSIXct(the_date)
  }
  if (lubridate::is.POSIXct(the_date)){
    the_date <- as.numeric(the_date)
  } else{
    stop("Not a POSIXct object or can't be coerced to it")
  }
  return(the_date)
}
