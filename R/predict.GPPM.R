#' @export
predict.GPPM <- function(theModel,newData){
##update model
IDfield <- attr(theModel$data,'ID')
predictionIDs <- unique(newData[,IDfield])
oldData <- theModel$data[theModel$data[,IDfield] %in% predictionIDs,]
dataUpdate <- rbind(oldData,newData)
newModel <- gppModel(theModel$mFormula,theModel$cFormula,dataUpdate)
newModel <- fit(newModel,useOptimizer=FALSE,init=as.list(coef(theModel)))
meansAndCovs <- fitted(newModel)
res <- list()
res$ID <- meansAndCovs$IDs
means <- meansAndCovs$mean
covs <- meansAndCovs$cov
for (i in 1:length(res$ID)){
  borderOld <- sum(oldData[,IDfield]==res$ID[i])
  nTimePoints <- sum(dataUpdate[,IDfield]==res$ID[i])

  indexOld <- 1:borderOld
  indexNew <- (borderOld+1):nTimePoints

  fullMean <- means[[i]]
  fullCov <- covs[[i]]
  mOld <- fullMean[indexOld]
  mNew <- fullMean[indexNew]

  cOld <- fullCov[indexOld,indexOld]
  cNew <- fullCov[indexNew,indexNew]

  cOldInv <- solve(cOld)

  crossNewOld <- fullCov[indexNew,indexOld]
  crossOldNew <- fullCov[indexOld,indexNew]
  stopifnot(all.equal(crossNewOld,t(crossOldNew),tolerance=1e-10)) #TODO why identical fail

  y <- as.matrix(theModel$dataForStan$Y[[i]][indexOld])
  #calculate
  res$predMean[[i]] <- mNew + crossNewOld %*% cOldInv %*% (y-mOld)
  res$predCov[[i]] <- cNew + crossNewOld %*% cOldInv %*% crossOldNew
}
class(res) <- 'GPPMPred'
res
}


