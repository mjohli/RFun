#' A string to numeric converter
#'
#' This resolves encoding issues and extracts a number of specified length from a string
#' @param x A character vector containing numeric information
#' @param n A single number or a vector containing lower and upper bound of the length of the expected number
#' @param words Boolean indicating whether number words should be converted to numbers (can increase calculation time)
#' @param strOut Boolean indicating whether output should be string or numeric
#' @param encodeCleaner Boolean indicating whether input should be cleaned to avoid errors by invalid bytecode
#' @param wtnErrorCatch Boolean indicating whether word to number conversion should be looped, use only if errors occur, weakens performance.
#' @export
#' @examples
#' stringToNum(c("abc", "abc123", "abc12de", "twelve"))
#'

stringToNum <- function(x, n = c(1, 2), words = TRUE, strOut = FALSE, encodeCleaner = TRUE, wtnErrorCatch = FALSE){
    if(encodeCleaner){
        x <- stringi::stri_trans_general(str = x, id = "latin-ascii")
    }
    if(words){
        if(sum(is.na(x)) > 0){
            warning("NAs in vector")
            x <- ifelse(is.na(x), "", x)
        }
        if(wtnErrorCatch){
            for(i in 1:length(x)){
                tryCatch(x[i] <- wordstonumbers::words_to_numbers(x[i]), error = function(e) cat(paste0(e, "i = ", i, "\nx[i] = ", x[i])))
            }
            x <- ifelse(is.null(x), NA, x)
        } else{
            x <- Vectorize(FUN = wordstonumbers::words_to_numbers)(x)
        }

    }
    x <- gsub(pattern = paste0("^\\D*(\\d{", paste(n, collapse = ","), "})(\\D.*)?$"), replacement = "\\1", x)
    if(!strOut){
        x <- as.numeric(x)
    }
    return(x)
}
