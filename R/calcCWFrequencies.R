#' Calculates geocode to geocode match
#'
#' Calculates how many geocodes of one level match to one geocode of the other level, gives out the number of different geocodes, the maximum number of geocodes matching to one geocode and a frequency plot.
#'
#' @param dat A crosswalk file from geocorr
#' @param geocode1 Name of the variable containing the first geocode
#' @param geocode2 Name of the variable containing the second geocode
#' @export
#'

calcCWFrequencies <- function(dat, geocode1, geocode2){
    plot <- function(pDat){
        ggplot2::ggplot(data = pDat) +
            ggplot2::aes(Freq) +
            ggplot2::geom_bar(color = "coral", fill = "red") +
            moTheme
    }
    res <- list(geo1 = list(n = length(unique(dat[[geocode1]]))),
                geo2 = list(n = length(unique(dat[[geocode2]]))))
    #L1:
    dat <- subset(dat, subset = dat[[geocode1]] %in% subset(dat, duplicated(dat[[geocode1]]))[[geocode1]])
    dat <- dat[order(dat[[geocode1]]),]
    d1 <- as.data.frame(table(dat[[geocode1]]))
    res[["geo1"]]["max"] <- max(d1$Freq)
    res[["geo1"]][["plot"]] <- plot(pDat = d1)
    #L2:
    d2 <- as.data.frame(table(dat[[geocode2]]))
    res[["geo2"]]["max"] <- max(d2$Freq)
    res[["geo2"]][["plot"]] <- plot(d2)
    return(res)
}
