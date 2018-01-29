#TODO rethink constructor, where to check what
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
    stanModel=NA,      #generated stan Model
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
  theModel$stanModel <- toStan(theModel$parsedModel,theModel$dataForStan)
  return(theModel)
}

#' @export
fit <-  function(theModel) {
  UseMethod("fit")
}

#' @export
fit.GPPM <-  function(theModel) {
  theModel$stanOut <- optimizing(theModel$stanModel,theModel$dataForStan,hessian = TRUE)
  #ML, StdErrors,
  theModel$fitRes <-
}

