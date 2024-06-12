#' Convenience function for updating RFun
#'
#' Unloads RFun, installs the newest version and attaches it again
#' @export
#'

updateRFun <- function(){
    unloadNamespace("RFun")
    devtools::install_github("mjohli/RFun")
    library(RFun)
}
