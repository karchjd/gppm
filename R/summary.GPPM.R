new_ModelSpecification<- function(meanFormula,covFormula,nPar){
  structure(list(
    meanFormula=meanFormula,
    covFormula=covFormula,
    nPar=nPar
  ),
  class='ModelSpecification'
  )
}

new_ModelFit <- function(AIC,BIC,logLik){
  structure(list(
    AIC=AIC,
    BIC=BIC,
    logLik=logLik
  ),
  class='ModelFit'
  )
}

new_DataStats <- function(nPer,maxTime,nPreds,nTime){
  structure(list(
    nPer=nPer, #number of persons
    maxTime=maxTime, #maximum number of time points
    nPreds=nPreds, #number of predictors
    nTime=nTime #number of time points per persons (vector)
  ),
  class='DataStats'
  )
}

new_summary.GPPM <- function(modelSpecification,parameterEstimates,modelFit,dataStats){
  structure(list(
    modelSpecification=modelSpecification,
    parameterEstimates=parameterEstimates, #output from parameterEsts()
    modelFit=modelFit,
    dataStats=dataStats
  ),
  class='summary.GPPM'
  )
}

#' @export
summary.GPPM <- function (x, ...) {
  modelSpecification <- new_ModelSpecification(meanf(x),covf(x),npar(x))
  parameterEstimates <- parameterEsts(x)
  modelfit <- new_ModelFit(AIC(x),BIC(x),logLik(x))
  dataStats <- new_DataStats(nobs(x),maxtime(x),npred(x),nstime(x))
  new_summary.GPPM(modelSpecification,parameterEstimates,modelfit,dataStats)
}

roundForPrint <- function(x){
  #constants
  rf <- function(x){round(x,2)}

  if(is.data.frame(x)){
    sel <- vapply(tmp,is.numeric,FUN.VALUE=TRUE)
    x[,sel] <- rf(x[,sel])
  }else{
    x <- rf(x)
  }
  x
}

#' @export
print.summary.GPPM <- function (summaryObj, ...) {
cat('Summary of Gaussian process panel model \n \n')
cat('Model Specification:','\n')
cat(sprintf('\t Mean Formula: \t\t %s \n',summaryObj$modelSpecification$meanFormula))
cat(sprintf('\t Covariance Formula: \t %s \n',summaryObj$modelSpecification$covFormula))
cat(sprintf('\t Number of Parameters: \t %s \n',summaryObj$modelSpecification$nPar))
cat('\n')
cat('Parameter Estimates:','\n')
print(roundForPrint(summaryObj$parameterEstimates),row.names = FALSE)
cat('\n')
cat('Model Fit:','\n')
cat(sprintf('\t AIC: \t\t\t %s \n', roundForPrint(summaryObj$modelFit$AIC)))
cat(sprintf('\t BIC: \t\t\t %s \n', roundForPrint(summaryObj$modelFit$BIC)))
cat(sprintf('\t Log Likelihood: \t %s \n', roundForPrint(summaryObj$modelFit$logLik)))
cat('\n')
cat('Data Set Characteristica:','\n')
cat(sprintf('\t Number of Persons: \t\t %s \n',summaryObj$dataStats$nPer))
cat(sprintf('\t Maximum Number of Timepoints: \t %s \n',summaryObj$dataStats$maxTime))
cat(sprintf('\t Number of Predictors: \t\t %s \n',summaryObj$dataStats$nPred))
sprintf('\t Number of Timepoints per Person: see nstime()')

}
