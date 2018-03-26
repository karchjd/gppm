#' Extract point estimates from a Gaussian process panel model
#'
#' @export
coef.GPPM <- function (model, ...){
  if (is.null(model$fitRes$paraEsts)){
    stop('Fit the model first using fit()')
  }else{
    model$fitRes$paraEsts
  }
}

#' @export
vcov.GPPM <- function (model, ...)
{
  model$fitRes$vcov
}


#' @export
SE <- function (model, ...){
  UseMethod("SE")
}

#' @export
SE.default <- function (model, ...)
{
  sqrt(diag(vcov(model, ...)))
}

#' @export
SE.GPPM <- function (model, ...)
{
  SE.default(model, ...) #here to have SE appear in extractors('GPPM') output
}

#' @export
confint.GPPM <- function (model, parm, level = 0.95, ...)
{
  cf <- coef(model)
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
  ses <- sqrt(diag(vcov(model)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}


#' @export
logLik.GPPM <- function(model){
  val <- -2*model$fitRes$minus2LL
  attr(val, "nobs") <- model$dataForStan$nPer
  attr(val, "df") <- model$fitRes$nPar
  class(val) <- "logLik"
  val
}

#' @export
fitted.GPPM <- function(model,..){
  list(mean=model$fitRes$mu,cov=model$fitRes$Sigma,IDs=attr(model$dataForStan,'IDs'))
}


#' @export
nobs.GPPM <- function (x, ...) {
  x$dataForStan$nPer
}

#' @export
npar <-  function(model) {
  UseMethod("npar")
}

#' @export
npar.GPPM <- function (x, ...) {
  length(x$parsedModel$params)
}

#' @export
maxtime <-  function(model) {
  UseMethod("maxtime")
}

#' @export
maxtime.GPPM <- function (x, ...) {
  x$dataForStan$maxTime
}

#' @export
nstime <-  function(model) {
  UseMethod("nstime")
}

#' @export
nstime.GPPM <- function (x, ...) {
  x$dataForStan$nTime
}

#' @export
npred <-  function(model) {
  UseMethod("npred")
}

#' @export
preds <-  function(model) {
  UseMethod("preds")
}


#' @export
preds.GPPM <-  function(model) {
  model$parsedModel$preds
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
variable.names.GPPM <- function (model, ...) {
    model$parsedModel$params
}

#' @export
paramEsts <- function (model, level=.95) {
  UseMethod("paramEsts")
}


#' @export
paramEsts.GPPM <- function (model, level=.95) {
  stopifnot(level<=1 & level>0)
  stopifnot(isFitted(model))
  res <- as.data.frame(matrix(nrow=model$fitRes$nPar,ncol=5))
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- stats:::format.perc(a, 3)
  names(res) <- c('Param','Est','SE',pct)
  paraNames <- variable.names(model)
  confInters <- confint(model)
  res[['Param']] <- paraNames
  res[['Est']] <- coef(model)[paraNames]
  res[['SE']] <- SE(model)[paraNames]
  res[[4]] <- confInters[,1][paraNames]
  res[[5]] <- confInters[,2][paraNames]
  res
}

#' @export
meanf <-  function(model) {
  UseMethod("meanf")
}

#' @export
meanf.GPPM <- function (x, ...) {
  x$mFormula
}

#' @export
covf <-  function(model) {
  UseMethod("covf")
}

#' @export
covf.GPPM <- function (x, ...) {
  x$cFormula
}

#' @export
getIntern <- function (model, value, ...) {
  UseMethod("getIntern")
}

#' @export
getIntern.GPPM <- function (model, value, ...) {
  switch(value,data=model$data,parsedmFormula=model$parsedModel$mFormula,parsedcFormula=model$parsedModel$kFormula,stanData=model$dataForStan,
         stanModel=model$stanModel,stanOut=model$stanOut)
}

#' @export
getData <- function (model) {
  UseMethod("getData")
}

#' @export
getData.GPPM <- function (model) {
  model$data
}

#' @export
getIntern.GPPM <- function (model, value, ...) {
  switch(value,data=model$data,parsedmFormula=model$parsedModel$mFormula,parsedcFormula=model$parsedModel$kFormula,stanData=model$dataForStan,
         stanModel=model$stanModel,stanOut=model$stanOut)
}

#' @export
isFitted <-  function(model) {
  UseMethod("isFitted")
}


#' @export
isFitted.GPPM <-  function(model) {
  class(model$fitRes)=="StanData"
}


#' @export
extractors <-  function(class) {
  theMethods <- methods(class=class)
  for (i in 1:length(theMethods)){

  }
}

