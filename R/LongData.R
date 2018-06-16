new_LongData <- function(myData,ID,DV){
  stopifnot(is.character(ID) && length(ID)==1)
  stopifnot(is.character(DV) && length(DV)==1)
  stopifnot(is.data.frame(myData))

  structure(myData,class=c('LongData',class(myData)),ID=ID,DV=DV)
}

as_LongData <- function(myData,ID,DV){
  if (!"LongData" %in% class(myData)){
    myData <- new_LongData(myData,ID,DV)
  }else if (!missing(ID)){
    attr(myData,'ID') <- ID
  }else if (!missing(DV)){
    attr(myData,'DV') <- DV
  }
  myData
}


getID <- function(longData){
  stopifnot("LongData" %in% class(longData))
  return(attr(longData,'ID'))
}

getDV <- function(longData){
  stopifnot("LongData" %in% class(longData))
  return(attr(longData,'DV'))
}

