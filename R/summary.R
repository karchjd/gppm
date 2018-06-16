new_ModelSpecification<- function(meanFormula,covFormula,nPars,params,nPreds,preds){
  structure(list(
    meanFormula=meanFormula,
    covFormula=covFormula,
    nPars=nPars,
    params=params,
    nPreds=nPreds,
    preds=preds
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

new_DataStats <- function(nPer,maxTime,nTime){
  structure(list(
    nPer=nPer, #number of persons
    maxTime=maxTime, #maximum number of time points
    nTime=nTime #number of time points per persons (vector)
  ),
  class='DataStats'
  )
}

new_summaryGPPM <- function(modelSpecification,parameterEstimates,modelFit,dataStats){
  structure(list(
    modelSpecification=modelSpecification,
    parameterEstimates=parameterEstimates, #output from parameterEsts()
    modelFit=modelFit,
    dataStats=dataStats
  ),
  class='summary.GPPM'
  )
}

#' Summarizing GPPM
#'
#' \code{summary} method for class 'GPPM'.
#' @inheritParams nPers
#' @return An object of classs "summary.GPPM", which is a list with 4 entries:
#' ' \itemize{
#'   \item \code{modelSpecification} an object of class 'ModelSpecification' describing the model as a list with the following entries
#'   \itemize{
#'       \item \code{meanFormula} formula for the mean function; output of \code{\link{meanf}}
#'       \item \code{covFormula} formula for the covariance function; output of \code{\link{covf}}
#'       \item \code{nPars} number of parameters; output of \code{\link{nPars}}
#'       \item \code{params} parameter names; output of \code{\link{pars}}
#'       \item \code{nPreds} number of predictors; output of \code{\link{nPreds}}
#'       \item \code{preds} predictors names; output of \code{\link{preds}}
#'   }
#'   \item \code{parameterEstimates} a data frame containing a summary of the parameter estimates; output of \code{\link{parEsts}}
#'   \item \code{modelfit} An object of class "ModelFit" describing the modelfit using a list with the following entries
#'   \itemize{
#'       \item \code{AIC} AIC of the model; output of \code{\link{AIC}}
#'       \item \code{BIC} BIC of the model; output of \code{\link{BIC}}
#'       \item \code{logLik} log-likelihood of the model; output of \code{\link{logLik}}
#'   }
#'   \item \code{dataStats} An object of class "DataStats" describing the data set using a list with the following entries
#'      \itemize{
#'       \item \code{nPer} number of persons; output of \code{\link{nPers}}
#'       \item \code{maxTime} maximum number of observations per person; output of \code{\link{maxnObs}}
#'       \item \code{nTime} number of observations for each person; output of \code{\link{nObs}}
#'   }
#' }
#' @export
summary.GPPM <- function (gpModel) {
    modelSpecification <- new_ModelSpecification(meanf(gpModel),covf(gpModel),nPars(gpModel),pars(gpModel),nPreds(gpModel),preds(gpModel))
    dataStats <- new_DataStats(nobs(gpModel),maxnObs(gpModel),nObs(gpModel))
  if (isFitted(gpModel)){
    parameterEstimates <- parEsts(gpModel)
    modelfit <- new_ModelFit(AIC(gpModel),BIC(gpModel),logLik(gpModel))
  }else{
    parameterEstimates <- modelfit <- NA
  }
    new_summaryGPPM(modelSpecification,parameterEstimates,modelfit,dataStats)
}

roundForPrint <- function(x){
  #constants
  rf <- function(x){round(x,2)}

  if(is.data.frame(x)){
    sel <- vapply(x,is.numeric,FUN.VALUE=TRUE)
    x[,sel] <- rf(x[,sel])
  }else{
    x <- rf(x)
  }
  x
}

#' @describeIn summary.GPPM Printing a summary.GPPM object
#' @export
print.summary.GPPM <- function (summaryObj, ...) {
cat('Summary of Gaussian process panel model \n \n')
cat('Model Specification:','\n')
cat(sprintf('\t Mean Formula: \t\t %s \n',summaryObj$modelSpecification$meanFormula))
cat(sprintf('\t Covariance Formula: \t %s \n',summaryObj$modelSpecification$covFormula))
cat(sprintf('\t Parameters: \t\t %s (%d)\n',paste(summaryObj$modelSpecification$params,collapse = ','),summaryObj$modelSpecification$nPars))
cat(sprintf('\t Predictors: \t\t %s (%d)\n',paste(summaryObj$modelSpecification$preds,collapse = ','),summaryObj$modelSpecification$nPreds))
cat('\n')
if (!is.na(summaryObj$parameterEstimates)){
  cat('Parameter Estimates:','\n')
  print(roundForPrint(summaryObj$parameterEstimates),row.names = FALSE)
  cat('\n')
}
if (!is.na(summaryObj$modelFit)){
  cat('Model Fit:','\n')
  cat(sprintf('\t AIC: \t\t\t %s \n', roundForPrint(summaryObj$modelFit$AIC)))
  cat(sprintf('\t BIC: \t\t\t %s \n', roundForPrint(summaryObj$modelFit$BIC)))
  cat(sprintf('\t Log Likelihood: \t %s \n', roundForPrint(summaryObj$modelFit$logLik)))
  cat('\n')
}
cat('Data Set Characteristica:','\n')
cat(sprintf('\t Number of Persons: \t\t %s \n',summaryObj$dataStats$nPer))
cat(sprintf('\t Maximum Number of Timepoints: \t %s \n',summaryObj$dataStats$maxTime))
sprintf('\t Number of Timepoints per Person: see nstime()')
}
