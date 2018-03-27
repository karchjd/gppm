#' Point Estimates
#'
#' Extracts point estimates for all parameters from a fitted GPPM.
#'
#' @param gpModel object of class GPPM. Must be fitted, that is, a result from \code{\link{fit.GPPM}}.
#' @family functions to extract from a GPPM
#' @return Point estimates for all parameters as a named numeric vector.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' simpleModel <- fit(simpleModel)
#' paraEsts <- coef(simpleModel)
#' @export
coef.GPPM <- function (gpModel){
  checkFitted(gpModel)
  gpModel$fitRes$paraEsts
}

#' Variance-Covariance Matrix
#'
#' Returns the variance-covariance matrix of the parameters of a fitted GPPM.
#'
#' @inheritParams coef.GPPM
#' @family functions to extract from a GPPM
#' @return A matrix of the estimated covariances between the parameter estimates. This has row and column names corresponding to the parameter names.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' simpleModel <- fit(simpleModel)
#' paraEsts <- vcov(simpleModel)
#' @export
vcov.GPPM <- function (gpModel)
{
  checkFitted(gpModel)
  gpModel$fitRes$vcov
}


#' Standard Errors
#'
#' Returns the standard errors of the parameters of a fitted GPPM.
#'
#' @inheritParams coef.GPPM
#' @family functions to extract from a GPPM
#' @return Standard errors for all parameters as a named numeric vector.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' simpleModel <- fit(simpleModel)
#' paraEsts <- SE(simpleModel)
#' @export
SE <- function (gpModel)
{
  checkGPPM(gpModel)
  checkFitted(gpModel)
  sqrt(diag(vcov(gpModel)))
}


#' Confidence Intervals
#'
#' Computes confidence intervals for one or more parameters in a fitted GPPM.
#' @inheritParams coef.GPPM
#' @family functions to extract from a GPPM
#' @param parm a specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level the confidence level required.
#'
#' @return A matrix (or vector) with columns giving lower and upper confidence limits for each parameter. These will be labelled as (1-level)/2 and 1 - (1-level)/2 in % (by default 2.5% and 97.5%).
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' simpleModel <- fit(simpleModel)
#' paraEsts <- confint(simpleModel)
#' @export
confint.GPPM <- function(gpModel, parm, level = 0.95)
{
  cf <- coef(gpModel) #checksfitted
  pnames <- names(cf)
  if (missing(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qnorm(a) #besides this line completely the same as confint.lm
  pct <- stats:::format.perc(a, 3)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                             pct))
  ses <- sqrt(diag(vcov(gpModel)))[parm]
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
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' simpleModel <- fit(simpleModel)
#' ll <- logLik(simpleModel)
#' @export
logLik.GPPM <- function(gpModel){
  checkFitted(gpModel)
  val <- -2*gpModel$fitRes$minus2LL
  attr(val, "nobs") <- gpModel$dataForStan$nPer
  attr(val, "df") <- gpModel$fitRes$nPar
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
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' simpleModel <- fit(simpleModel)
#' meansCovs <- fitted(simpleModel)
#'
#' person1Mean <- meansCovs$mean[[1]]
#' person1Cov <- meansCovs$cov[[1]]
#' person1ID <- meansCovs$ID[[1]]
#' @export
fitted.GPPM <- function(gpModel,..){
  checkFitted(gpModel)
  list(mean=gpModel$fitRes$mu,cov=gpModel$fitRes$Sigma,ID=attr(gpModel$dataForStan,'IDs'))
}

#' Number of persons
#'
#' Extracts the number of persons from a GPPM.
#'
#' @family functions to extract from a GPPM
#' @param gpModel object of class GPPM.
#' @return Number of persons as a numeric.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' simpleModel <- fit(simpleModel)
#' numberPersons <- nobs(simpleModel)
#' @export
nobs.GPPM <- function (gpModel) {
  gpModel$dataForStan$nPer
}



#' Number of Parameters
#'
#' Extracts the number of parameters from a GPPM.
#'
#' @inheritParams nobs.GPPM
#' @family functions to extract from a GPPM
#' @return Number of parameters as a numeric.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' numberParas <- npar(simpleModel)
#' @export
npar <- function (gpModel) {
  checkGPPM(gpModel)
  length(gpModel$parsedModel$params)
}


#' Maximum Number of Observations per Person
#'
#' Extracts the maximum number of observations per person from a GPPM.
#'
#' @inheritParams nobs.GPPM
#' @family functions to extract from a GPPM
#' @return Maximum number of observations as a numeric.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' numberTimePoints <- maxtime(simpleModel)
#' @export
maxntime <- function (gpModel){
  checkGPPM(gpModel)
  gpModel$dataForStan$maxTime
}

#' Number of Observations
#'
#' Extracts the number of observations for each person from a GPPM.
#'
#' @inheritParams nobs.GPPM
#' @family functions to extract from a GPPM
#' @return Number of observations for each person as a numeric vector. The corresponding IDs are in the IDs attribute.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' numberTimePoints <- nstime(simpleModel)
#'
#' @export
nstime <- function (gpModel) {
  checkGPPM(gpModel)
  result <- gpModel$dataForStan$nTime
  attr(result) <- attr(gpModel$dataForStan,'IDs')
}




#' Number of Predictors
#'
#' Extracts the number of predictors from a GPPM.
#'
#' @inheritParams nobs.GPPM
#' @family functions to extract from a GPPM
#' @return Number of predictors as numeric.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' nPreds <- npred(simpleModel)
#' @export
npred <- function (gpModel) {
  checkGPPM(gpModel)
  gpModel$dataForStan$nPreds
}

#' Predictors Names
#'
#' Extracts the predictors from a GPPM
#'
#' @inheritParams nobs.GPPM
#' @family functions to extract from a GPPM
#' @return The names of the predictors.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' myPreds <- preds(simpleModel)
#' @export
preds <-  function(gpModel) {
  checkGPPM(gpModel)
  gpModel$parsedModel$preds
}


#' Parameter Names
#'
#' Extracts the parameter names from a GPPM.
#'
#' @inheritParams nobs.GPPM
#' @family functions to extract from a GPPM
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' parameters <- variable.names(simpleModel)
#' @return The names of the paramters
#' @export
variable.names.GPPM <- function (gpModel) {
    gpModel$parsedModel$params
}


#' Essential Parameter Estimation Results
#'
#' Extracts the essential parameter estimation results for a GPPM.
#'
#' @inherit coef.GPPM params seealso
#' @family functions to extract from a GPPM
#' @param level The confidence level required.
#' @return The names of the parameters
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' paramEssentials <- paramEsts(simpleModel)
#'
#' @return A data.frame containing the estimated parameters, standard errors, and the lower and upper bounds of the confidence intervals.
#' @export
paramEsts <- function (gpModel, level=.95) {
  stopifnot(level<=1 & level>0)
  checkFitted(gpModel)
  res <- as.data.frame(matrix(nrow=gpModel$fitRes$nPar,ncol=5))
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- stats:::format.perc(a, 3)
  names(res) <- c('Param','Est','SE',pct)
  paraNames <- variable.names(gpModel)
  confInters <- confint(gpModel)
  res[['Param']] <- paraNames
  res[['Est']] <- coef(gpModel)[paraNames]
  res[['SE']] <- SE(gpModel)[paraNames]
  res[[4]] <- confInters[,1][paraNames]
  res[[5]] <- confInters[,2][paraNames]
  res
}


#' Mean Function
#'
#' Extracts the mean function from a GPPM.
#'
#' @inheritParams nobs.GPPM
#' @family functions to extract from a GPPM
#' @return The mean function as a string.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' myMean <- meanf(simpleModel)
#' @export
meanf <- function (gpModel) {
  checkGPPM(gpModel)
  gpModel$mFormula
}


#' Covariance Function
#'
#' Extracts the covariance function from a GPPM.
#'
#' @inheritParams nobs.GPPM
#' @family functions to extract from a GPPM
#' @return The covariance function as a string.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' myCov <- covf(simpleModel)
#' @export
covf <- function (gpModel, ...) {
  checkGPPM(gpModel)
  gpModel$cFormula
}


#' Data Set
#'
#' Extracts the data set from a GPPM.
#'
#' @inheritParams nobs.GPPM
#' @family functions to extract from a GPPM
#' @return The data set associated with the GPPM
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' myData <- datas(simpleModel)
#' @export
datas <- function (gpModel) {
  checkGPPM(gpModel)
  gpModel$data
}


#' Generic Extraction Function
#'
#' Extracts internals from a GPPM.
#'
#' @inheritParams nobs.GPPM
#' @param quantity name of the quanity to extract. Possible values are
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
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' myData <- datas(simpleModel)
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
