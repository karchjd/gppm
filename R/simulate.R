#' XXXX
#' @examples
#' data("demoLGCM")
#' lgcm <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
#'         demoLGCM,'ID','x')
#'
#' parameterValues <- c(10,-1,0,10,0,0.1)
#' names(parameterValues) <-c('muI','muS','varI','varS','covIS','sigma')
#' simData <- simulate(lgcm,parameterValues)
#' @import MASS
#' @export
simulate.GPPM <- function (gpModel, parameterValues, seed = NULL){
    validate_simulate(gpModel,parameterValues)

    ##set seed
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

    ##core
    gpModel <- fit(gpModel,useOptimizer=FALSE,init=parameterValues)
    meansAndCovs <- fitted(gpModel)
    IDs <- meansAndCovs$IDs
    simData <- getIntern(gpModel,'data')
    idCol <- attr(simData,'ID')
    dvCol <- attr(simData,'DV')
    simData[,dvCol] <- NA
    for (i in seq_len(length(IDs))){
      cMu <- meansAndCovs$mean[[i]]
      cCov <- meansAndCovs$cov[[i]]
      simulated <- MASS::mvrnorm(mu=cMu,Sigma=cCov)
      simData[simData[,idCol]==IDs[i],dvCol] <- simulated
    }
    simData
}

validate_simulate <- function (gpModel, parameterValues){
  stopifnot(class(gpModel)=='GPPM')
  stopifnot(is.numeric(parameterValues))

  allParas <- variable.names(gpModel)
  specified <- names(parameterValues)
  specifiedInModel <- specified %in% allParas
  if (!(all(specifiedInModel))){
    stop(sprintf('The parameters %s were specified but are not in the model',paste(specified[!specifiedInModel],collapse = ',')))
  }
  modelInSpecified <- allParas %in%  specified
  if (!(all(modelInSpecified))){
    stop(sprintf('The parameters %s are in the model but were not specified',paste(allParas[!modelInSpecified],collapse = ',')))
  }
}
