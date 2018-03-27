#' Extract GPPM Point Estimates
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
  model$fitRes$paraEsts
}

#' Calculate Variance-Covariance Matrix for a GPPM
#'
#' Returns the variance-covariance matrix of the parameters of a fitted GPPM.
#'
#' @inherit coef.GPPM params seealso family
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

#' Calculate Standard Errors
#'
#' Returns the standard errors of the main parameters of a fitted model object.
#'
#' @param object a fitted model object.
#' @return Standard errors for all parameters as a named numeric vector.
#' @export
SE <- function (object){
  UseMethod("SE")
}


#' @export
SE.default <- function (gpModel)
{

}


#' Calculate  Standard Errors for a GPPM
#'
#' Returns the standard errors of the parameters of a fitted GPPM.
#'
#' @inherit coef.GPPM params seealso
#'
#' @return Standard errors for all parameters as a named numeric vector.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' simpleModel <- fit(simpleModel)
#' paraEsts <- SE(simpleModel)
#' @export
SE.GPPM <- function (gpModel, ...)
{
  checkFitted(gpModel)
  SE.default(gpModel) #here to have SE appear in extractors('GPPM') output
}


#' Confidence Intervals for GPPM Parameters
#'
#' Computes confidence intervals for one or more parameters in a fitted GPPM.
#' @inherit coef.GPPM params seealso
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
confint.GPPM <- function(gpModel, parm, level = 0.95, ...)
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


#' Extract Log-Likelihood from GPPMs
#'
#' Compute the log-likelihood for a GPPM at the maximum likelihood parameter values.
#'
#' @inherit coef.GPPM params seealso
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

#' Extract fitted person-specific mean vectors and covariance matrices
#'
#' A fitted GPPM implies a mean vector and a covariance matrix for each person. These are returned by this function.
#' @inherit coef.GPPM params seealso
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

#' Extract the Number of persons from a GPPM
#'
#' Extracts the number of persons from a GPPM.
#' @inherit coef.GPPM seealso
#' @param gpModel object of class GPPM.
#' @return Number of persons as a numeric.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' simpleModel <- fit(simpleModel)
#' numberPersons <- nobs(simpleModel)
#' @export
nobs.GPPM <- function (gpModel, ...) {
  gpModel$dataForStan$nPer
}


#' Extract Number of Parameters from a Model
#'
#' Extracts the number of parameters from a model.
#'
#' @param object a statistical model
#' @return Number of parameters as a numeric.
#' @export
npar <-  function(object) {
  UseMethod("npar")
}

#' Extract the Number of Parameters from a GPPM
#'
#' Extracts the number of parameters from a GPPM
#'
#' @inherit nobs.GPPM params seealso
#' @return Number of parameters as a numeric.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' numberPersons <- nobs(simpleModel)
#' @export
npar.GPPM <- function (gpModel, ...) {
  length(gpModel$parsedModel$params)
}

#' Extract the Maximum Number of Observations per Person from a Longitudinal Model
#'
#' Extracts the maximum number of Observations per Person from a longitudinal model.
#' @param object a longitudinal model
#' @seealso \code{\link{maxtime.GPPM}}
#' @export
maxtime <-  function(object) {
  UseMethod("maxtime")
}


#' Extract the Maximum Number of Observations per Person from a GPPM
#'
#' Extracts the maximum number of observations per person from a GPPM.
#'
#' @inherit nobs.GPPM params seealso
#' @return Maximum number of observations as a numeric.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' numberTimePoints <- maxtime(simpleModel)
#' @export
maxtime.GPPM <- function (gpModel, ...) {
  gpModel$dataForStan$maxTime
}


#' Extract the Number of Observations from a Longitudinal Model
#'
#' Extracts the number of observations for each person from a longitudinal model.
#'
#' @inheritParams maxtime
#' @return Number of observations for each person as a numeric vector.
#' @seealso \code{\link{nstime.GPPM}}
#' @export
nstime <-  function(object) {
  UseMethod("nstime")
}

#' Extract the Number of Observations from a GPPM
#'
#' Extracts the number of observations for each person from a GPPM.
#'
#' @inherit nobs.GPPM params seealso
#' @return Number of observations for each person as a numeric vector. The corresponding IDs are in the IDs attribute.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' numberTimePoints <- nstime(simpleModel)
#'
#' @export
nstime.GPPM <- function (gpModel, ...) {
  result <- gpModel$dataForStan$nTime
  attr(result) <- attr(gpModel$dataForStan,'IDs')
}


#' Extract the Number of Predictors
#'
#' Extracts the number of predictors from a model.
#'
#' @param object a  model
#' @return Number of predictors as numeric.
#' @seealso \code{\link{npred.GPPM}}
#' @export
npred <-  function(object) {
  UseMethod("npred")
}


#' Extract the Number of Predictors
#'
#' Extracts the number of predictors from a GPPM.
#'
#' @param gpModel object of class GPPM.
#' @return Number of predictors as numeric.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' nPreds <- npred(simpleModel)
#' @export
npred.GPPM <- function (gpModel, ...) {
  gpModel$dataForStan$nPreds
}

#' Extract the Predictors
#'
#' Extracts the predictors from a model.
#'
#' @param object a model using predictors.
#' @return The names of the predictors.
#' @seealso \code{\link{preds.GPPM}}
#' @export
preds <-  function(gpModel) {
  UseMethod("preds")
}


#' Extract the Predictors
#'
#' Extracts the predictors from a GPPM
#'
#' @inherit nobs.GPPM params seealso
#' @return The names of the predictors.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' myPreds <- preds(simpleModel)
#' @export
preds.GPPM <-  function(gpModel) {
  gpModel$parsedModel$preds
}


#' Extract the Parameter names
#'
#' Extracts the parameter names from a GPPM.
#'
#' @inherit nobs.GPPM params seealso
#' @return The names of the parameters.
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' parameters <- variable.names(simpleModel)
#' @return The variable names of the predictors
#' @export
variable.names.GPPM <- function (gpModel, ...) {
    gpModel$parsedModel$params
}


#' Extract the Essential Parameter Estimation Results
#'
#' Extracts the essential parameter estimation results for a model
#'
#' @param object statistical model.
#' @return Essential parameter estimation results.
#' @export
paramEsts <- function (object, ...) {
  UseMethod("paramEsts")
}

#' Extract the Essential Parameter Estimation Results
#'
#' Extracts the essential parameter estimation results for a GPPM
#'
#' @inherit coef.GPPM params seealso
#' @level The confidence level required.
#' @return The names of the parameters
#' @examples
#' data("demoLGCM")
#' simpleModel <- gppm('grandMean','(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' parameters <- variable.names(simpleModel)
#'
#' @return A data.frame containing the estimated parameters, standard errorsm and the lower and upper bounds of the confidence intervals.
#' @export
paramEsts.GPPM <- function (gpModel, level=.95) {
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

#' @export
meanf <-  function(gpModel) {
  UseMethod("meanf")
}

#' @export
meanf.GPPM <- function (gpModel, ...) {
  gpModel$mFormula
}

#' @export
covf <-  function(gpModel) {
  UseMethod("covf")
}

#' @export
covf.GPPM <- function (gpModel, ...) {
  gpModel$cFormula
}

#' @export
getIntern <- function (gpModel, value, ...) {
  UseMethod("getIntern")
}

#' @export
getIntern.GPPM <- function (gpModel, value, ...) {
  switch(value,data=gpModel$data,parsedmFormula=gpModel$parsedModel$mFormula,parsedcFormula=gpModel$parsedModel$kFormula,stanData=gpModel$dataForStan,
         stanModel=gpModel$stanModel,stanOut=gpModel$stanOut)
}

#' @export
getData <- function (gpModel) {
  UseMethod("getData")
}

#' @export
getData.GPPM <- function (gpModel) {
  gpModel$data
}






isFitted <-  function(gpModel) {
  class(gpModel$fitRes)=="StanData"
}

checkFitted <- function(gpModel){
  if (!isFitted(gpModel)){
    stop('Model must be fitted using fit() first!')
  }
}

#' Foo bar generic
#'
#' @param x Object to foo.
#' @export
foobar <- function(x) UseMethod("foobar")

#' @describeIn foobar Difference between the mean and the median
#' @export
foobar.numeric <- function(x) abs(mean(x) - median(x))

#' @describeIn foobar First and last values pasted together in a string.
#' @export
foobar.character <- function(x) paste0(x[1], "-", x[length(x)])
