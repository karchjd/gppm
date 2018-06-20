
#' Point Estimates
#'
#' Extracts point estimates for all parameters from a fitted GPPM.
#'
#' @param object object of class GPPM. Must be fitted, that is, a result from \code{\link{fit.GPPM}}.
#' @param ... additional arguments (currently not used).
#' @family functions to extract from a GPPM
#' @return Point estimates for all parameters as a named numeric vector.
#' @examples
#' data("exampleModel")
#' paraEsts <- coef(exampleModel)
#' @export
coef.GPPM <- function (object,...){
  checkFitted(object)
  object$fitRes$paraEsts
}

#' Variance-Covariance Matrix
#'
#' Returns the variance-covariance matrix of the parameters of a fitted GPPM.
#'
#' @inheritParams coef.GPPM
#' @family functions to extract from a GPPM
#' @return A matrix of the estimated covariances between the parameter estimates. This has row and column names corresponding to the parameter names.
#' @examples
#' data("exampleModel")
#' covMat <- vcov(exampleModel)
#' @export
vcov.GPPM <- function (object,...)
{
  checkFitted(object)
  object$fitRes$vcov
}


#' Standard Errors
#'
#' Returns the standard errors of the parameters of a fitted GPPM.
#'
#' @inheritParams coef.GPPM
#' @family functions to extract from a GPPM
#' @return Standard errors for all parameters as a named numeric vector.
#' @examples
#' data("exampleModel")
#' stdErrors <- SE(exampleModel)
#' @export
SE <- function (object)
{
  checkGPPM(object)
  checkFitted(object)
  sqrt(diag(vcov(object)))
}


#' Confidence Intervals
#'
#' Computes confidence intervals for one or more parameters in a fitted GPPM.
#' @inheritParams coef.GPPM
#' @family functions to extract from a GPPM
#' @param parm vector of strings. The parameters for which confidence intervals are desired. If missing, confidence intervals for all parameters are returned.
#' @param level scalar from 0 to 1. The confidence level required.
#'
#' @return A matrix (or vector) with columns giving lower and upper confidence limits for each parameter. These will be labelled as (1-level)/2 and 1 - (1-level)/2 in \% (by default 2.5\% and 97.5\%).
#' @examples
#' data("exampleModel")
#' confInts <- confint(exampleModel)
#' @export
confint.GPPM <- function(object, parm, level = 0.95,...)
{
  cf <- coef(object) #checksfitted
  pnames <- names(cf)
  if (missing(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qnorm(a) #besides this line completely the same as confint.lm
  pct <- format.perc(a, 3)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                             pct))
  ses <- sqrt(diag(vcov(object)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}


#' Log-Likelihood
#'
#' Compute the log-likelihood for a GPPM at the maximum likelihood parameter values.
#'
#' @inheritParams coef.GPPM
#' @family functions to extract from a GPPM
#' @return Returns an object of class logLik. Attributs are: "df" (\strong{d}egrees of \strong{f}reedom; number of estimated parameters in the model) and nobs (number of persons in the model)
#' @examples
#' data("exampleModel")
#' ll <- logLik(exampleModel)
#' @export
logLik.GPPM <- function(object,...){
  checkFitted(object)
  val <- object$fitRes$LL
  attr(val, "nobs") <- object$dataForStan$nPer
  attr(val, "df") <- object$fitRes$nPar
  class(val) <- "logLik"
  val
}

#' Person-specific mean vectors and covariance matrices
#'
#' A fitted GPPM implies a mean vector and a covariance matrix for each person. These are returned by this function.
#' @inheritParams coef.GPPM
#' @family functions to extract from a GPPM
#' @return Returns a list structure with mean and covariances matrices. See example.
#' @examples
#' data("exampleModel")
#' meansCovs <- fitted(exampleModel)
#'
#' person1Mean <- meansCovs$mean[[1]]
#' person1Cov <- meansCovs$cov[[1]]
#' person1ID <- meansCovs$ID[[1]]
#' @export
fitted.GPPM <- function(object,...){
  checkFitted(object)
  list(mean=object$fitRes$mu,cov=object$fitRes$Sigma,ID=attr(object$dataForStan,'IDs'))
}

#' Number of persons
#'
#' Extracts the number of persons from a GPPM.
#'
#' @family functions to extract from a GPPM
#' @param gpModel object of class GPPM.
#' @return Number of persons as a numeric.
#' @examples
#' data("exampleModel")
#' numberPersons <- nPers(exampleModel)
#' @export
nPers <- function (gpModel) {
  gpModel$dataForStan$nPer
}



#' Number of Parameters
#'
#' Extracts the number of parameters from a GPPM.
#'
#' @inheritParams nPers
#' @family functions to extract from a GPPM
#' @return Number of parameters as a numeric.
#' @examples
#' data("exampleModel")
#' numberParas <- nPars(exampleModel)
#' @export
nPars <- function (gpModel) {
  checkGPPM(gpModel)
  length(gpModel$parsedModel$params)
}


#' Maximum Number of Observations per Person
#'
#' Extracts the maximum number of observations per person from a GPPM.
#'
#' @inheritParams nPers
#' @family functions to extract from a GPPM
#' @return Maximum number of observations as a numeric.
#' @examples
#' data("exampleModel")
#' maxNumberObs <- maxnObs(exampleModel)
#' @export
maxnObs <- function (gpModel){
  checkGPPM(gpModel)
  gpModel$dataForStan$maxTime
}

#' Number of Observations
#'
#' Extracts the number of observations for each person from a GPPM.
#'
#' @inheritParams nPers
#' @family functions to extract from a GPPM
#' @return Number of observations for each person as a numeric vector. The corresponding IDs are in the IDs attribute.
#' @examples
#' data("exampleModel")
#' numberObs <-  nObs(exampleModel)
#'
#' @export
nObs <- function (gpModel) {
  checkGPPM(gpModel)
  result <- gpModel$dataForStan$nTime
  attr(result,'IDs') <- attr(gpModel$dataForStan,'IDs')
  result
}




#' Number of Predictors
#'
#' Extracts the number of predictors from a GPPM.
#'
#' @inheritParams nPers
#' @family functions to extract from a GPPM
#' @return Number of predictors as numeric.
#' @examples
#' data("exampleModel")
#' numberPreds <- nPreds(exampleModel)
#' @export
nPreds <- function (gpModel) {
  checkGPPM(gpModel)
  gpModel$dataForStan$nPreds
}

#' Predictors Names
#'
#' Extracts the predictor names from a GPPM.
#'
#' @inheritParams nPers
#' @family functions to extract from a GPPM
#' @return The names of the predictors.
#' @examples
#' data("exampleModel")
#' myPreds <- preds(exampleModel)
#' @export
preds <-  function(gpModel) {
  checkGPPM(gpModel)
  gpModel$parsedModel$preds
}


#' Parameter Names
#'
#' Extracts the parameter names from a GPPM.
#'
#' @inheritParams nPers
#' @family functions to extract from a GPPM
#' @examples
#' data("exampleModel")
#' parameters <- pars(exampleModel)
#' @return The names of the paramters
#' @export
pars <- function (gpModel) {
    gpModel$parsedModel$params
}


#' Essential Parameter Estimation Results
#'
#' Extracts the essential parameter estimation results for a GPPM.
#'
#' @inherit coef.GPPM params seealso
#' @family functions to extract from a GPPM
#' @param level scalar from 0 to 1. The confidence level required.
#' @examples
#' data("exampleModel")
#' paramEssentials <- parEsts(exampleModel)
#'
#' @return A data.frame containing the estimated parameters, standard errors, and the lower and upper bounds of the confidence intervals.
#' @export
parEsts <- function (object, level=.95) {
  stopifnot(level<=1 & level>0)
  checkFitted(object)
  res <- as.data.frame(matrix(nrow=object$fitRes$nPar,ncol=5))
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- format.perc(a, 3)
  names(res) <- c('Param','Est','SE',pct)
  paraNames <- pars(object)
  confInters <- confint(object)
  res[['Param']] <- paraNames
  res[['Est']] <- coef(object)[paraNames]
  res[['SE']] <- SE(object)[paraNames]
  res[[4]] <- confInters[,1][paraNames]
  res[[5]] <- confInters[,2][paraNames]
  res
}


#' Mean Function
#'
#' Extracts the mean function from a GPPM.
#'
#' @inheritParams nPers
#' @family functions to extract from a GPPM
#' @return The mean function as a character string.
#' @examples
#' data("exampleModel")
#' myMean <- meanf(exampleModel)
#' @export
meanf <- function (gpModel) {
  checkGPPM(gpModel)
  gpModel$mFormula
}


#' Covariance Function
#'
#' Extracts the covariance function from a GPPM.
#'
#' @inheritParams nPers
#' @family functions to extract from a GPPM
#' @return The covariance function as a character string.
#' @examples
#' data("exampleModel")
#' myCov <- covf(exampleModel)
#' @export
covf <- function (gpModel) {
  checkGPPM(gpModel)
  gpModel$cFormula
}


#' Data Set
#'
#' Extracts the data set from a GPPM.
#'
#' @inheritParams nPers
#' @family functions to extract from a GPPM
#' @return The data set associated with the GPPM.
#' @examples
#' data("exampleModel")
#' myData <- datas(exampleModel)
#' @export
datas <- function (gpModel) {
  checkGPPM(gpModel)
  gpModel$data
}


#' Generic Extraction Function
#'
#' Extracts internals from a GPPM.
#'
#' @inheritParams nPers
#' @param quantity character string. Name of the quanity to extract. Possible values are
#'\itemize{
#'   \item "parsedmFormula" for the parsed mean formula
#'   \item "parsedcFormula" for the parsed covariance formula
#'   \item "stanData" for the data set in the form needed for rstan
#'   \item "stanModel" for the created rstan model
#'   \item "stanOut" for the created stan output
#' }
#' @family functions to extract from a GPPM
#' @return The requested quantity
#' @examples
#' data("exampleModel")
#' getIntern(exampleModel,'parsedmFormula')
#' @export
getIntern <- function (gpModel, quantity) {
  checkGPPM(gpModel)
  if (quantity %in% ('stanOut')){
    checkFitted(gpModel)
  }
  switch(quantity,parsedmFormula=gpModel$parsedModel$mFormula,parsedcFormula=gpModel$parsedModel$kFormula,stanData=gpModel$dataForStan,
         stanModel=gpModel$stanModel,stanOut=gpModel$stanOut,stop(sprintf('Unkown quantity %s',quantity)))
}

isFitted <-  function(gpModel) {
  class(gpModel$fitRes)=="StanData"
}

isGPPM <-  function(gpModel) {
  inherits(gpModel,'GPPM')
}

checkFitted <- function(gpModel){
  if (!isFitted(gpModel)){
    stop('gpModel must be fitted using fit() first!')
  }
}

checkGPPM <- function(gpModel){
  if (!isGPPM(gpModel)){
    stop('gpModel must inherit \'GPPM\'')
  }
}

#copied from interal stats function
format.perc <- function (probs, digits)
  paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
        "%")
