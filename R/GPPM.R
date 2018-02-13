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
fit.GPPM <-  function(theModel,init='random',useOptimizer=TRUE) {
  if (useOptimizer)
    iter<-2000
  else{
    iter <- 0
  }
  theModel$stanOut <- optimizing(theModel$stanModel,theModel$dataForStan,hessian = TRUE,init=init,iter=iter)
  theModel$fitRes <- extractFitRes(theModel$stanOut,theModel$parsedModel,theModel$dataForStan[c('nPer','nTime','maxTime')])
  theModel
}

#' @export
# TODO, idea without input simulate data for predictors as in data (easy)
# with new input simulate data for that input (probably hard)
simulate.GPPM <- function (object, nsim = 1, seed = NULL, ...)
{
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    runif(1)
  if (is.null(seed))
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  fam <- if (inherits(object, "glm"))
    object$family$family
  else "gaussian"
  ftd <- fitted(object)
  isMlm <- identical(fam, "gaussian") && is.matrix(ftd)
  nm <- if (isMlm)
    dimnames(ftd)
  else names(ftd)
  if (isMlm)
    stop("simulate() is not yet implemented for multivariate lm()")
  n <- length(ftd)
  ntot <- n * nsim
  val <- switch(fam, gaussian = {
    vars <- deviance(object)/df.residual(object)
    if (isMlm) {
    } else {
      if (!is.null(object$weights)) vars <- vars/object$weights
      ftd + rnorm(ntot, sd = sqrt(vars))
    }
  }, if (!is.null(object$family$simulate)) object$family$simulate(object,
                                                                  nsim) else stop(gettextf("family '%s' not implemented",
                                                                                           fam), domain = NA))
  if (isMlm) {
  }
  else if (!is.list(val)) {
    dim(val) <- c(n, nsim)
    val <- as.data.frame(val)
  }
  else class(val) <- "data.frame"
  names(val) <- paste0("sim_", seq_len(nsim))
  if (!is.null(nm))
    row.names(val) <- nm
  attr(val, "seed") <- RNGstate
  val
}


