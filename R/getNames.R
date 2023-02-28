#' A function that connects series and character names
#'
#' This gives the character names appearing in a given series and vice versa
#' @param name The name of either a series character or series title
#' @param lowerCase Indicates whether the given and the returned names should be all lowercase. Default is 'None' making no lower case, 'Series' and 'Characters' give and take series title and characters respectively in lower case, 'Both' gives and takes everything in lower case.
#' @export
#' @examples
#' covTex(c(1,2,3), c(2,3,4))
#'

getNames <- Vectorize(function(name, lowerCase = "None"){
  series <- list("HIMYM" = c("Ted", "Lily", "Marshall", "Barney", "Robin"),
                 "Friends" = c("Monica", "Rachel", "Phoebe", "Ross", "Chandler", "Joey"),
                 "TBBT" = c("Sheldon", "Howard", "Leonard", "Penny", "Raj", "Amy", "Bernadette"),
                 "Office" = c("Dwight", "Angela", "Michael", "Jim", "Andy", "Pam"))
  if(lowerCase %in% c("Both", "Characters")){
    series <- lapply(series, function(x)tolower(x))
  }
  if(lowerCase %in% c("Both", "Series")){
    names(series) <- tolower(names(series))
  }
  sName <- names(series)[unlist(lapply(series, function(x){name %in% x}))]
  if(name %in% names(series)){
    return(series[[name]])
  } else{
    sName <- names(series)[unlist(lapply(series, function(x){name %in% x}))]
    if(length(sName)){
      return(sName)
    } else{
      stop(paste0("No character or series named '", name, "'"))
    }
  }
})
