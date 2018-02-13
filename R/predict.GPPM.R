# preds <- predict.GPPM(gpModel,1,newData)
#' @export
predictGPPM- function(gpModel,i,newData){
  #update model
  dataUpdate <- cbind(gpModel$data[i,],newData)
  newModel <- gppModel(gpModel$mf,gpModel$cf,dataUpdate)
  newModel <- gppSetStart(newModel,names(gpModel$mlParas),gpModel$mlParas)
  newModel<- gppFit(newModel,useOptimizer=FALSE)

  ##get mean and covariance matrices
  Ysel <- rep(FALSE,ncol(gpModel$data))
  Ysel[grep("Y[[:digit:]]+",names(gpModel$data))] <- TRUE #mark Y cols
  availOldYs <- Ysel & !is.na(gpModel$data[i,]) #mark non na Y cols
  oldYs <- names(gpModel$data)[availOldYs]
  newYs <- names(newData)[grep("Y[[:digit:]]+",names(newData))]

  #mean old
  meanCov <- mxGetExpected(newModel$omx,c('means','covariance'),defvar.row = i)
  browser()
  fullMean <- meanCov$means
  fullCov <- meanCov$covariance

  mOld <- t(t(fullMean[1,oldYs]))
  mNew <- t(t(fullMean[1,newYs]))

  cOld <- fullCov[oldYs,oldYs]
  cNew <- fullCov[newYs,newYs]

  cOldInv <- solve(cOld)

  crossNewOld <- fullCov[newYs,oldYs]
  crossOldNew <- fullCov[oldYs,newYs]
  browser()
  stopifnot(identical(crossNewOld,t(crossOldNew)))

  y <- t(gpModel$data[i,oldYs])
  #calculate
  predMean <- mNew + crossNewOld %*% cOldInv %*% (y-mOld)
  predCov <- cNew + crossNewOld %*% cOldInv %*% crossOldNew
  resList <- list(predMean=predMean,predCov=predCov)
  class(resList) <- 'GPPM.pred'
  return(resList)
}

#' @export
predict.GPPM <- function(theModel,newData){
##update model
IDfield <- attr(theModel$data,'ID')
predictionIDs <- unique(newData[,IDfield])
oldData <- theModel$data[theModel$data[,IDfield] %in% predictionIDs,]
dataUpdate <- rbind(oldData,newData)
newModel <- gppModel(theModel$mFormula,theModel$kFormula,dataUpdate)
newModel <- fit(newModel,useOptimizer=FALSE,init=as.list(coef(theModel)))

meansAndCovs <- fitted(newModel)
res <- list() # TODO init
for
mOld <- t(t(fullMean[1,oldYs]))
mNew <- t(t(fullMean[1,newYs]))

cOld <- fullCov[oldYs,oldYs]
cNew <- fullCov[newYs,newYs]

cOldInv <- solve(cOld)

crossNewOld <- fullCov[newYs,oldYs]
crossOldNew <- fullCov[oldYs,newYs]
browser()
stopifnot(identical(crossNewOld,t(crossOldNew)))

y <- t(gpModel$data[i,oldYs])
#calculate
predMean <- mNew + crossNewOld %*% cOldInv %*% (y-mOld)
predCov <- cNew + crossNewOld %*% cOldInv %*% crossOldNew
resList <- list(predMean=predMean,predCov=predCov)
class(resList) <- 'GPPM.pred'
return(resList)
}
