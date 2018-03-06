#' @export
coef.GPPM <- function (object, ...){
  if (is.null(object$fitRes$paraEsts)){
    stop('Fit the model first using fit()')
  }else{
    object$fitRes$paraEsts
  }
}

#' @export
coef.GPPM <- function (object, ...){
  if (is.null(object$fitRes$paraEsts)){
    stop('Fit the model first using fit()')
  }else{
    object$fitRes$paraEsts
  }
}

#' @export
vcov.GPPM <- function (object, ...)
{
  object$fitRes$vcov
}

#' @export
vcov.GPPM <- function (object, ...)
{
  object$fitRes$vcov
}


#' @export
SE <- function (object, ...){
  UseMethod("SE")
}

#' @export
SE.default <- function (object, ...)
{
  sqrt(diag(vcov(object, ...)))
}

#' @export
confint.GPPM <- function (object, parm, level = 0.95, ...)
{
  cf <- coef(object)
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
  ses <- sqrt(diag(vcov(object)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}


#' @export
logLik.GPPM <- function(object){
  val <- -2*object$fitRes$minus2LL
  attr(val, "nobs") <- object$dataForStan$nPer
  attr(val, "df") <- object$fitRes$nPar
  class(val) <- "logLik"
  val
}

#' @export
fitted.GPPM <- function(object,..){
  list(mean=object$fitRes$mu,cov=object$fitRes$Sigma,IDs=attr(object$dataForStan,'IDs'))
}


#' @export
nobs.GPPM <- function (x, ...) {
  x$dataForStan$nPer
}

#' @export
npar <-  function(theModel) {
  UseMethod("npar")
}

#' @export
npar.GPPM <- function (x, ...) {
  length(x$parsedModel$params)
}

#' @export
maxtime <-  function(theModel) {
  UseMethod("maxtime")
}

#' @export
maxtime.GPPM <- function (x, ...) {
  x$dataForStan$maxTime
}

#' @export
nstime <-  function(theModel) {
  UseMethod("nstime")
}

#' @export
nstime.GPPM <- function (x, ...) {
  x$dataForStan$nTime
}

#' @export
npred <-  function(theModel) {
  UseMethod("npred")
}

#' @export
preds <-  function(theModel) {
  UseMethod("preds")
}


#' @export
preds.GPPM <-  function(theModel) {
  theModel$parsedModel$preds
}
#' @export
npred.GPPM <- function (x, ...) {
  x$dataForStan$nPreds
}

#' @export
case.names.GPPM <- function (x, ...) {
  x$dataForStan$nPer
}

#' @export
variable.names.GPPM <- function (object, ...) {
    object$parsedModel$params
}

#' @export
variable.names.GPPM <- function (object, ...) {
  object$parsedModel$params
}

#' @export
parameterEsts <- function (object, level=.95) {
  stopifnot(level<=1 & level>0)
  stopifnot(isFitted(object))
  res <- as.data.frame(matrix(nrow=object$fitRes$nPar,ncol=5))
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- stats:::format.perc(a, 3)
  names(res) <- c('Param','Est','SE',pct)
  paraNames <- variable.names(object)
  confInters <- confint(object)
  res[['Param']] <- paraNames
  res[['Est']] <- coef(object)[paraNames]
  res[['SE']] <- SE(object)[paraNames]
  res[[4]] <- confInters[,1][paraNames]
  res[[5]] <- confInters[,2][paraNames]
  res
}

#' @export
meanf <-  function(theModel) {
  UseMethod("meanf")
}

#' @export
meanf.GPPM <- function (x, ...) {
  x$mFormula
}

#' @export
covf <-  function(theModel) {
  UseMethod("covf")
}

#' @export
covf.GPPM <- function (x, ...) {
  x$cFormula
}

#' @export
getIntern <- function (theModel, value, ...) {
  UseMethod("getIntern")
}

#' @export
getIntern.GPPM <- function (theModel, value, ...) {
  switch(value,data=theModel$data,parsedmFormula=theModel$parsedModel$mFormula,parsedcFormula=theModel$parsedModel$kFormula,stanData=theModel$dataForStan,
         stanModel=theModel$stanModel,stanOut=theModel$stanOut)
}

#' @export
isFitted <-  function(theModel) {
  UseMethod("isFitted")
}


#' @export
isFitted.GPPM <-  function(theModel) {
  class(theModel$fitRes)=="StanData"
}
