#' @export
gppFit <- function(gpModel,...){
  stopifnot(class(gpModel)=='GPPM')
  #fit
  fittedModel <- gpModel
  fittedModel$omx <- mxModel(fittedModel$omx,mxCI(names(c(omxGetParameters(fittedModel$omx)))))
  fittedModel$omx <- mxRun(fittedModel$omx,silent=TRUE,unsafe=TRUE,intervals = TRUE,...)

  #get ML pars out
  omxParas <- omxGetParameters(fittedModel$omx)
  names(omxParas)<-gsub('GPPM.|\\[1,1\\]','',names(omxParas))
  fittedModel$mlParas <- omxParas[names(fittedModel$mlParas)]
  fittedModel$mll <- summary(fittedModel$omx)$Minus2LogLikelihood
  return(fittedModel)
}

