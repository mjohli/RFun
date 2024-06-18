#' !!Preliminary!! function to extract times from the GPIPP!!
#'
#' First steps to extract times and time zones from older answers to the GPIPP since it has different date formats
#' @param x The date string
#' @export
#'

getDateTz <- function(x){
    tzs <- substr(x, 21, 23)
    x <- paste0(substr(x, 5, 20), substr(x, 25, 29))
    lubridate::parse_date_time(x, orders = "bdHMSY", tz = tzs)#Lubridate not yet in DESCRIPTION
}
