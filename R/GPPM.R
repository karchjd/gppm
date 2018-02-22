new_GPPM <- function(mFormula,cFormula,myData){
  stopifnot(is.character(mFormula))
  stopifnot(is.character(cFormula))
  stopifnot(is.data.frame(myData))

  structure(list(
    mFormula=mFormula, #formula for the mean
    cFormula=cFormula, #formula for the covariance
    data=myData,       #data must be a data frame
    parsedModel=NA,    #model in a parsed format
    dataForStan=NA,    #data as used for stan
    stanModel=NA,      #generated stan Model
    stanOut =NA,       #stan output
    fitRes=NA          #all the fitting results
  ),class='GPPM')
}

#' Define a Gaussian process panel model
#'
#' This function is used to specify a Gaussian process panel model,
#' which can then be fit using \code{\link{fit}}.
#'
#' @param mFormula character string. Contains the specification of the mean function. See details for more information.
#'
#' @param cFormula character string. Contains the specification of the covariance function. See details for more information.
#'
#' @param myData data frame. Contains the data to which the model is fitted. Must be in the long-format.
#'
#' @param ID character string. Contains the column label in myData which describes the subject ID.
#'
#' @param DV character string. Contains the column label in myData which contains the to be modeled variable.
#'
#' @return a (unfitted) Gaussian process panel model, that is an object of class 'GPPM'
#' @seealso \code{\link{fit.GPPM}} for how to fit a GPPM
#' @examples
#' # Defintion of a latent growth curve model
#' data("demoLGCM")
#' lgcm <- gppModel('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
#'         demoLGCM,'ID','x')
#' @import rstan
#' @import Rcpp
#' @export
gppModel <- function(mFormula,cFormula,myData,ID,DV){
  if (!"longData" %in% class(myData)){
    myData <- structure(myData,class=c('longData',class(myData)),ID=ID,DV=DV)
  }

  theModel <- new_GPPM(mFormula,cFormula,myData)
  theModel$dataForStan <- as_StanData(myData)
  theModel$parsedModel <- parseModel(theModel$mFormula,theModel$cFormula,theModel$dataForStan)
  theModel$stanModel <- toStan(theModel$parsedModel,theModel$dataForStan)
  return(theModel)
}

#' @export
fit <-  function(theModel,...) {
  UseMethod("fit")
}

#' @export
fit.GPPM <-  function(theModel,init='random',useOptimizer=TRUE) {
  if (useOptimizer)
    iter<-2000
  else{
    iter <- 0
  }
  theModel$stanOut <- rstan::optimizing(theModel$stanModel,theModel$dataForStan,hessian = TRUE,iter=iter,init=init)
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


