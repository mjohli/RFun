#' A function that connects series and character names
#'
#' This gives the character names appearing in a given series and vice versa
#' @param name The name of either a series character or series title
#' @param lowerCase Indicates whether the given and the returned names should be all lowercase. Default is 'None' making no lower case, 'Series' and 'Characters' give and take series title and characters respectively in lower case, 'Both' gives and takes everything in lower case.
#' @export
#' @examples
#' getNames(name = "HIMYM")
#' getNames("Ted")
#' getNames("ted", lowerCase = "both")
#'

getNames <- Vectorize(FUN = function(name, lowerCase = "None"){
    getNam <- function(x){
        names(x)[unlist(lapply(x, FUN = function(x) name %in% x))]
    }
    series <- list("HIMYM" = c("Ted", "Lily", "Marshall", "Barney", "Robin"),
                   "Friends" = c("Monica", "Rachel", "Phoebe", "Ross", "Chandler", "Joey"),
                   "TBBT" = c("Sheldon", "Howard", "Leonard", "Penny", "Raj", "Amy", "Bernadette"),
                   "Office" = c("Dwight", "Angela", "Michael", "Jim", "Andy", "Pam"))
    if(lowerCase %in% c("Both", "Characters")){
        series <- lapply(series, tolower)
    }
    if(lowerCase %in% c("Both", "Series")){
        names(series) <- tolower(names(series))
    }
    sName <- getNam(series)
    if(name %in% names(series)){
        return(series[[name]])
    } else{
        sName <- getNam(series)
      if(length(sName)){
        return(sName)
      } else{
        stop(paste0("No character or series named '", name, "'"))
      }
    }
})
