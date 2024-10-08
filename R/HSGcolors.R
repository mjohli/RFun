#' HSG colors
#'
#' A vector containing the HSG corporate identity colors.
#' @param ... Name(s) of HSG colors to retrieve, if empty: returns all names and values, if "all": returns only values to use
#' @returns A vector containing values of the specified colors
#' @examples
#' HSGcolors()
#' HSGcolors("HSGgreen", "HSGblue")
#' HSGcolors("all")
#' @export

HSGcolors <- function(...){
    colors <- c(HSGgreen = "#00802f", HSGdarkgreen = "#0a5f2d",
                   HSGbeige = "#e1d7c3", HSGcoral = "#eb6969",
                   HSGblue = "#73a5af", HSGyellow = "#fff04b")
    if(missing(...)){
        return(colors)
    }
    if("all" %in% c(...)){
        return(unname(colors))
    }
    return(unname(colors[match(c(...), names(colors))]))
}
