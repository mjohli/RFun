#' Function to extract times from the GPIPP
#'
#' Converts to date and converts the more difficult strings in a second step if necessary
#' @param x Vector full of date strings
#' @export
#'

getDateTz <- function(x){
    res <- as.POSIXct(rep(NA, length(x)))
    for(i in 1:length(x)){
        v <- try(as.POSIXct(x[i]), silent = TRUE)
        if("try-error" %in% class(v)){
            tz <- gsub("^[A-Z][a-z]{2}\\s[A-Z][a-z]{2}\\s{1,2}\\d{1,2}\\s\\d{2}:\\d{2}:\\d{2}\\s([A-Z]{3})\\s\\d{3,4}$", "\\1", x[i])
            m <- match(gsub("^[A-Z][a-z]{2}\\s([A-Z][a-z]{2})\\s{1,2}\\d{1,2}\\s\\d{2}:\\d{2}:\\d{2}\\s[A-Z]{3}\\s\\d{3,4}$", "\\1", x[i]), month.abb)
            tidyd <- paste0(gsub("^[A-Z][a-z]{2}\\s[A-Z][a-z]{2}\\s{1,2}\\d{1,2}\\s\\d{2}:\\d{2}:\\d{2}\\s[A-Z]{3}\\s(\\d{3,4})$", "\\1-", x[i]), m,
                            gsub("^[A-Z][a-z]{2}\\s[A-Z][a-z]{2}\\s{1,2}(\\d{1,2})\\s(\\d{2}:\\d{2}:\\d{2})\\s[A-Z]{3}\\s\\d{3,4}$", "-\\1 \\2", x[i]))
            tidyd <- gsub("-(\\d)-", "-0\\1-", tidyd)
            v <- try(as.POSIXct(tidyd, tz = tz), silent = TRUE)
            if("try-error" %in% class(v)){
                v <- NA
            }
        }
        res[i] <- as.POSIXct(v)
    }
    return(res)
}

