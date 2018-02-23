#' @export
predict.GPPM <- function(theModel,newData){
##update model
oldData <- getIntern(theModel,'data')
IDfield <- attr(oldData,'ID')
predictionIDs <- unique(newData[,IDfield])

oldData <- oldData[oldData[,IDfield] %in% predictionIDs,]
dataUpdate <- rbind(oldData,newData)
newModel <- gppModel(meanf(theModel),covf(theModel),dataUpdate)
newModel <- fit(newModel,useOptimizer=FALSE,init=as.list(coef(theModel)))
meansAndCovs <- fitted(newModel)
res <- list()
res$ID <- meansAndCovs$IDs
means <- meansAndCovs$mean
covs <- meansAndCovs$cov
for (i in seq_len(length(res$ID))){
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

  y <- as.matrix(getIntern(theModel,'stanData')[[i]][indexOld])
  #calculate
  res$predMean[[i]] <- mNew + crossNewOld %*% cOldInv %*% (y-mOld)
  res$predCov[[i]] <- cNew + crossNewOld %*% cOldInv %*% crossOldNew
}
class(res) <- 'GPPMPred'
res
}


