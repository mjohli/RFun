#' Function to extract times from the GPIPP
#'
#' Converts to date and converts the more difficult strings in a second step if necessary
#' @param x Vector full of date strings
#' @export
#'
getDateTz <- function(x){
    tryPOSI <- function(a, tz = ""){
        a <- try(as.POSIXct(a), silent = TRUE)
        if("try-error" %in% class(a)){
            return(NA)
        }else{
            return(a)
        }
    }
    res <- as.POSIXct(sapply(x, tryPOSI))
    x <- x[is.na(res)]
    tzs <- gsub("^[A-Z][a-z]{2}\\s[A-Z][a-z]{2}\\s{1,2}\\d{1,2}\\s\\d{2}:\\d{2}:\\d{2}\\s([A-Z]{3})\\s\\d{3,4}$", "\\1", x)
    tzs <- ifelse(tzs == "EST", tzs, ifelse(tzs == "EDT", "America/Detroit", NA))
    ms <- match(gsub("^[A-Z][a-z]{2}\\s([A-Z][a-z]{2})\\s{1,2}\\d{1,2}\\s\\d{2}:\\d{2}:\\d{2}\\s[A-Z]{3}\\s\\d{3,4}$", "\\1", x), month.abb)
    tidyds <- paste0(gsub("^[A-Z][a-z]{2}\\s[A-Z][a-z]{2}\\s{1,2}\\d{1,2}\\s\\d{2}:\\d{2}:\\d{2}\\s[A-Z]{3}\\s(\\d{3,4})$", "\\1-", x), ms,
                     gsub("^[A-Z][a-z]{2}\\s[A-Z][a-z]{2}\\s{1,2}(\\d{1,2})\\s(\\d{2}:\\d{2}:\\d{2})\\s[A-Z]{3}\\s\\d{3,4}$", "-\\1 \\2", x))
    mat <- matrix(c(tzs, tidyds), ncol = 2)
    x <- apply(mat, 1, function(x)tryPOSI(x[2], x[1]))
    res[is.na(res)] <- x
    return(res)
}
