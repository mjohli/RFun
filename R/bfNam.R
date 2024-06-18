#' A function that gives names of big five dimensions
#'
#' Gives names of interest, maybe extended
#' @param letter Boolean indicating whether a letter or the short form is needed
#' @param lowercase Boolean indicating whether dimension names should be all lowercase or title case
#' @export
#' @examples
#' bfNam()
#' bfNam(TRUE)
#' bfNam(TRUE, FALSE)
#' bfNam(lowercase = FALSE)
#'

bfNam <- function(letter = FALSE, lowercase = TRUE){
    if(letter){
        bf <- c("O", "C", "E", "A", "N")
    } else{
        bf <- c("Opn", "Cns", "Ext", "Agr", "Neu")
    }
    if(lowercase){
        return(tolower(bf))
    } else{
        return(bf)
    }
}
