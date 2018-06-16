 #' GPPM predictions
#'
#' Predict values based on a GPPM
#'
#' @inheritParams coef.GPPM
#' @param newData a data frame with the same column names as the data frame used for generating \code{gpModel} with \code{\link{gppm}}
#' @return Predictions of the dependent variable for all rows in newData. Conditional predictions for all persons in newData that are also present
#' in the data used for fitting gpModel; unconditional predictions for others persons.
#' See examples for format.
#'
#' @examples
#'data("demoLGCM")
#' #remove all measurements from person 1 and the first form person 2
#' predIdx <- c(which(demoLGCM$ID==1),which(demoLGCM$ID==2)[1])
#' fitDemoLGCM <- demoLGCM[setdiff(1:nrow(demoLGCM),predIdx),]
#'
#' lgcm <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
#'              fitDemoLGCM,'ID','y')
#' lgcm <- fit(lgcm)
#' predictions <- predict(lgcm,demoLGCM[predIdx,])
#' @export
predict.GPPM <- function(gpModel,newData,...){
checkFitted(gpModel)

#get relevant old data
oldData <- datas(gpModel)
IDfield <- getID(oldData)
predictionIDs <- unique(newData[,IDfield])
oldData <- oldData[oldData[,IDfield] %in% predictionIDs,]
dataUpdate <- rbind(oldData,newData)

#get model implied mean and covariance matrices for all relevant persons
newModel <- updateData(gpModel,dataUpdate)
newModel <- fit(newModel,useOptimizer=FALSE,init=coef(gpModel),...)
meansAndCovs <- fitted(newModel)

#calculate predictions
res <- list(predMean=list(),predCov=list(),type=character(),preds=list(),trueVals=list())
res$ID <- meansAndCovs$ID
means <- meansAndCovs$mean
covs <- meansAndCovs$cov
for (i in seq_len(length(res$ID))){


  borderOld <- sum(oldData[,IDfield]==res$ID[i])
  nTimePoints <- sum(dataUpdate[,IDfield]==res$ID[i])

  indexOld <- seq_len(borderOld)
  indexNew <- (borderOld+1):nTimePoints
  onlyDataForPersonI <- dataUpdate[dataUpdate[,IDfield]==res$ID[i],]
  res$preds[[i]] <- onlyDataForPersonI[indexNew,preds(gpModel),drop=FALSE]
  res$trueVals[[i]] <- onlyDataForPersonI[indexNew,attr(oldData,'DV'),drop=FALSE]
  fullMean <- means[[i]]
  fullCov <- covs[[i]]

  mNew <- fullMean[indexNew]
  cNew <- fullCov[indexNew,indexNew,drop=FALSE]
  #we have measurements for this person
  if (!length(indexOld)==0){
    mOld <- fullMean[indexOld]
    cOld <- fullCov[indexOld,indexOld,drop=FALSE]
    cOldInv <- solve(cOld)

    crossNewOld <- fullCov[indexNew,indexOld,drop=FALSE]
    crossOldNew <- fullCov[indexOld,indexNew,drop=FALSE]
    stopifnot(all.equal(crossNewOld,t(crossOldNew),tolerance=1e-10)) #TODO why identical fail

    y <- as.matrix(getIntern(newModel,'stanData')$Y[[i]][indexOld])
    #calculate
    res$predMean[[i]] <- mNew + crossNewOld %*% cOldInv %*% (y-mOld)
    res$predCov[[i]] <- cNew - crossNewOld %*% cOldInv %*% crossOldNew
    res$type[i] <- 'conditional'
  }else{
    res$predMean[[i]] <- mNew
    res$predCov[[i]] <- cNew
    res$type[i] <- 'unconditional'
  }
}
class(res) <- 'GPPMPred'
res
}

#' @export
accuracy <- function(predRes){
  nPers <- length(predRes$ID)
  MSE <- c()
  LL <- rep(NA,nPers)
  for (i in 1:nPers){
    MSE <- c(MSE,(predRes$predMean[i]-predRes$trueVals[[i]])^2)
    LL[i] <- log(mvtnorm::dmvnorm(t(predRes$trueVals[[i]]),mean=as.vector(predRes$predMean[[i]]),sigma=predRes$predCov[[i]]))
  }
  mMSE <- mean(MSE)
  sLL <- sum(LL)
  return(list(MSE=mMSE,nLPP=-sLL))
}

#' @export
plot.GPPMPred <- function(predictions,plotId,trainingData=NULL){
  require(ggthemes)
  stopifnot(length(plotId)==1)
  idIdx = plotId==predictions$ID
  stopifnot(sum(idIdx)==1)
  #only for one predictor
  #mean
  toPlot <- data.frame(mypreds=predictions$preds[[idIdx]],means=predictions$predMean[[idIdx]],var=diag(predictions$predCov[[idIdx]]),trueV=predictions$trueVals[[idIdx]])
  thePlot <- ggplot2::ggplot(toPlot, ggplot2::aes_string(x=names(toPlot)[1], y='means')) +
  ggplot2::geom_line(aes(y=means+1.96*var))+
  ggplot2::geom_line(aes(y=means-1.96*var))+
  geom_ribbon(aes( ymax=means+1.96*var, ymin=means-1.96*var), fill="grey", alpha=.5)+
  ggplot2::geom_line(size=1)
  if (!is.null(trainingData)){
    idcol <- attr(trainingData,'ID')
    trainingData <- trainingData[trainingData[,idcol]==plotId,]
    dataPlot <- data.frame(mypreds=trainingData[,attr(trainingData,'preds')],trueV=trainingData[,attr(trainingData,'DV')])
    thePlot <- thePlot + ggplot2::geom_point(data=dataPlot,mapping=aes(x=mypreds,y=trueV))
  }
  thePlot <- thePlot + ggthemes::theme_tufte()+xlab(paste0('Predictor ',names(toPlot)[1]))+ylab(paste0('Outcome ', attr(trainingData,'DV')))

}




