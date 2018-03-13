#' @export
fit <-  function(theModel,...) {
  UseMethod("fit")
}

#' Fit a Gaussian process panel model
#'
#' This function is used to fit a Gaussian process panel model,
#' which has been specified fit using \code{\link{gppm}}.
#'
#' @param gpModel object of class GPPM. The Gaussian proces spanel model to be fitted.
#'
#' @param init string or named numeric vector. Used to specify the starting values for the parameters. Can either be the string 'random' (default) or a numeric vector startVal of starting values. Which value belongs to which parameter is determined by the names attribute of startVal. See also the example.
#'
#' @param useOptimizer boolean. Should the optimizer be used or not? For false the (possibly random) starting values are returned as the maximum likelihood values.
#'
#' @return a fitted Gaussian process panel model, which is an object of class 'GPPM'
#' @examples
#' #regular usage
#' data("demoLGCM")
#' linearChange <- gppm('muI+muS*t','(t==t#)*sigma',
#'         demoLGCM,'ID','x')
#' linearChange <- fit(linearChange)
#'
#' #starting values as ML results
#' startVals <- c(10,1,10)
#' names(startVals) <- c('muI','muS','sigma')
#' linearChangeFake <- fit(linearChange,init=startVals,useOptimizer=FALSE)
#' stopifnot(identical(startVals,coef(linearChangeFake)))
#' @export
fit.GPPM <-  function(gpModel,init='random',useOptimizer=TRUE,verbose=TRUE) {
  if (useOptimizer){
    iter<- 10000
    algorithm <- 'LBFGS' #default
  }
  else{
    iter <- 0
    algorithm <- 'Newton'#because for some reason LBFSGS still does an iteration even if iter==0
  }
  if (!init[1]=='random'){
    validate_simulate(gpModel,init)
    init = as.list(init)
  }
  if(verbose){

  }else{
    capture.output(gpModel$stanOut <- rstan::optimizing(gpModel$stanModel,gpModel$dataForStan,hessian = TRUE,iter=iter,init=init,algorithm=algorithm,as_vector=FALSE))

  }
  gpModel$fitRes <- extractFitRes(gpModel$stanOut,gpModel$parsedModel,gpModel$dataForStan[c('nPer','nTime','maxTime')])
  gpModel
}
