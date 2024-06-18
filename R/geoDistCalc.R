#' Calculates distances between all given shapes
#'
#' Calculates direct distances between every pairing of the given shapes. If necessary, simulates random variables for each geographic entity and gives them in a data frame.
#' @param sf Shapefile to calculate distances on
#' @param vars Name(s) of the variables that should be simulated
#' @returns Gives a matrix containing all distances between all pairings. If variables are simulated, gives a list containing the distances and a data frame with the variables together with the GEOID.
#' @export
#'

geoDistCalc <- function(sf, vars = NULL){
    cbsaShp <- sf::read_sf(sf)
    cbsaShp <- subset(cbsaShp, select = "GEOID")
    cbsaShp <- sf::st_centroid(cbsaShp)
    cbsaDistances <- as.data.frame(sf::st_distance(cbsaShp))
    colnames(cbsaDistances) <- rownames(cbsaDistances) <- cbsaShp$GEOID
    if(!is.null(vars)){
        cbsaShp <- sf::st_drop_geometry(cbsaShp)
        cbsaShp[vars] <- matrix(rnorm(length(cbsaShp$GEOID) * length(vars)), ncol = length(vars))
        return(list(df = cbsaShp, dists = cbsaDistances))
    } else{
        return(cbsaDistances)
    }
}
