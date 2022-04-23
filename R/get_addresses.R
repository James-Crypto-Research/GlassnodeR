#' Get all addresses statistics during the period
#'
#' @param asset Asset to examine (see glassnode api docs for a list)
#' @param since The start date
#' @param until The end date
#' @param frequency The frequency to examine: 1h,24h,10m,1w,1month
#' @param api_key The api key. It looks in the env variable GN_API_KEY if not set
#' @param as_date A logical to return a Date not a POSIXct object for daily or lower frequency
#'
#' @return return a tibble with the date and the addresses
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_addresses()
#' }
get_addresses <- function(asset="BTC",since=NULL,until=NULL,frequency="24h",
                                   api_key = Sys.getenv("GN_API_KEY"),
                                   as_date=TRUE) {
  active_a <-get_active_addresses(asset=asset,since=since,until=until,
                                  frequency=frequency,api_key=api_key,
                                  as_date=as_date)
  total_a <-get_total_addresses(asset=asset,since=since,until=until,
                                frequency=frequency,api_key=api_key,
                                as_date=as_date)
  send_a<-get_sending_addresses(asset=asset,since=since,until=until,
                                frequency=frequency,api_key=api_key,
                                as_date=as_date)
  rec_a <-get_receiving_addresses(asset=asset,since=since,until=until,
                                  frequency=frequency,api_key=api_key,
                                  as_date=as_date)
  dep_a <-get_depositing_addresses(asset=asset,since=since,until=until,
                                   frequency=frequency,api_key=api_key,
                                   as_date=as_date)
  new_a <- get_new_addresses(asset=asset,since=since,until=until,
                             frequency=frequency,api_key=api_key,as_date=as_date)
  with_a <-get_withdrawing_addresses(asset=asset,since=since,until=until,
                                     frequency=frequency,api_key=api_key,
                                     as_date=as_date)
  x <- plyr::join_all(list(active_a,total_a,send_a,rec_a,dep_a,with_a,new_a),
                      by="date") |> as_tibble()
  return(x)

}
