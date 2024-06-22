#' Converts red-green scales to red-blue scales
#'
#' Function allows easy conversion to red-green blind friendly color scale.
#' @param path The path where the image can be found
#' @param output The path where the converted image should be stored.
#' @export
#'

redGreenConverter <- function(path = file.choose(), output = path){
    pic <- png::readPNG(path)
    jpeg::writeJPEG(pic[ , , c(1,3,2)], output)
}
