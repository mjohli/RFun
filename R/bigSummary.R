#' Generates an extensive summary of a numerical variable
#'
#' This prints the name of the variable, a summary and a density plot
#' @param dvar The column of the data frame to investigate
#' @export
#'

bigSummary <- function(dvar){
    library(ggplot2)
    nam <- deparse(expr = substitute(expr = dvar))
    print(nam)
    print(summary(object = dvar))
    ggplot() +
        aes(dvar) +
        geom_density() +
        xlab(label = nam) +
        moTheme
}
