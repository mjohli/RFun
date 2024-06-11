#' IPUMS variable codes generator
#'
#' This generates IPUMS variable codes based on letter/number code(s) and number(s)
#' @param code The 3 letter code of the IPUMS variable, the fourth letter indicates whether it's estimate (E) or margin of error (M)
#' @param n The number of the IPUMS variable that comes after the letter code. It's sufficient to give the number without the leading zeros.
#' @param type The type of value wanted: "estimate" for Estimate, "margin" for Margin of Error, misspecify for further information on possible values., if NULL, only `code` is used.
#' @export
#' @examples
#' ipumsVars(code = "ABC", n = 1:3)
#' ipumsVars(c("ABC", "DEF"), 1, type = "margin")
#' try(ipumsVars("ABC", 1, "wrong"))
#'

ipumsVars <- function(code, n, type = "estimate"){
    warn <- "Usually the codes should contain three letters. The last letter defines whether it's estimate or margin of error."
    if(length(unique(nchar(code))) != 1){
        warning(paste0("Multiple codes with different lengths given. ", warn))
    } else if(nchar(code)[1] != 3){
        warning(warn)
    }
    e <- c("estimate", "e")
    m <- c("margin", "error", "margin of error", "error-margin",
           "error margin", "m")
    if(is.null(type)){
        type == ""
    } else if(tolower(type) %in% e){
        type <- "E"
    } else if(tolower(type) %in% m){
        type <- "M"
    } else{
        stop(paste0("No acceptable specification of value type. Acceptable types are (not case-sensitive):\nEstimate: ",
                    paste(e, collapse = ", "), "\nMargin of Error: ",
                    paste(m, collapse = ", ")))
    }
    return(paste0(code, type, formatC(n, digits = 2, format = "d", flag = "0")))
}
