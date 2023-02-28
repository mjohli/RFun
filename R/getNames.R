getNames <- function(name, lowerCase = "None"){
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
  if(name %in% names(series)){
    return(series[[name]])
  } else{
    sName <- names(series)[unlist(lapply(series, function(x){name %in% x}))]
    if(length(sName)){
      stop("No character or series name like this exists.")
    } else{
      return(sName)
    }
  }
}
