#' Generates an extensive summary of a numerical variable
#'
#' This prints the name of the variable, a summary and a density plot
#' @param x The column of the data frame to investigate
#' @export
#'

bigSummary <- function(x){
    library(ggplot2)
    nam <- deparse(expr = substitute(expr = x))
    print(nam)
    print(summary(object = x))
    ggplot() +
        aes(x) +
        geom_density() +
        xlab(label = nam) +
        moTheme
}
