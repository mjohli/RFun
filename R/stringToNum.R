#' A string to numeric converter
#'
#' This resolves encoding issues and extracts a number of specified length from a string
#' @param x A character vector containing numeric information
#' @param n A single number or a vector containing lower and upper bound of the length of the expected number
#' @param words Boolean indicating whether number words should be converted to numbers (can increase calculation time)
#' @param strOut Boolean indicating whether output should be string or numeric
#' @param encodeCleaner Boolean indicating whether input should be cleaned to avoid errors by invalid bytecode
#' @export
#' @examples
#' stringToNum(c("abc", "abc123", "abc12de", "twelve"))
#'

stringToNum <- function(x, n = c(1, 2), words = TRUE, strOut = FALSE, encodeCleaner = TRUE){
    if(encodeCleaner){
        x <- stringi::stri_trans_general(str = x, id = "latin-ascii")
    }
    if(words){
        x <- Vectorize(FUN = words_to_numbers)(x)
    }
    x <- gsub(pattern = paste0("^\\D*(\\d{", paste(n, collapse = ","), "})(\\D.*)?$"), replacement = "\\1", x)
    if(!strOut){
        x <- as.numeric(x)
    }
    return(x)
}
