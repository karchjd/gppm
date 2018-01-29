new_GPPM <- function(mFormula,kFormula,myData){
  stopifnot(is.character(mFormula))
  stopifnot(is.character(kFormula))
  stopifnot(is.data.frame(myData))

  structure(list(
    mFormula=mFormula, #formula for the mean
    kFormula=kFormula, #formula for the covariance
    data=myData,       #data must be a data frame
    parsedModel=NA,    #model in a parsed format
    dataForStan=NA,    #data as used for stan
    stanCode=NA,       #generated stan code
    stanOut =NA,       #stan output
    fitRes=NA          #all the fitting results
  ),class='GPPM')
}

#' @export
#' @import rstan
gppModel <- function(mFormula,kFormula,myData){
  theModel <- new_GPPM(mFormula,kFormula,myData)
  theModel$dataForStan <- as_StanData(myData)
  theModel$parsedModel <- parseModel(theModel$mFormula,theModel$kFormula,theModel$dataForStan)
  browser()
  #
  # stuffForStan <- toStan(theModel$parsedModel,theModel$data)
  #
  # theModel$stanCode <- stuffForStan$stanCode
  #
  #
  # theModel$stanOut <- fitStan(theModel$stanCode,theModel$dataForStan)
  #
  # theModel$fitRes <- fromStan(theModel$stanOut)

  return(theModel)
}



