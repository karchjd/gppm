validate_predict <- function(gpModel,newData){
  checkFitted(gpModel)

  #check for repeats of oldData in newData
  oldData <- datas(gpModel)
  oldData <- unique(oldData)
  newData <- unique(newData)
  data_all <- rbind(oldData, newData)

  if(anyDuplicated(data_all)){
    stop('newData contains data that was used for fitting the model')
  }
}


#' GPPM predictions
#'
#' Obtain person-specific predictions.
#'
#' @inheritParams coef.GPPM
#' @param newData a data frame with the same column names as the data frame used for generating \code{gpModel} with \code{\link{gppm}}. May only contain new data, that is, data that was not used for fitting.
#' @return Predictions of the dependent variable for all rows in newData. Conditional predictions for all persons in newData that are also present
#' in the data used for fitting gpModel; unconditional predictions for others persons.
#' See examples for format.
#'
#' @examples
#' \dontrun{
#'data("demoLGCM")
#' #remove all measurements from person 1 and the first form person 2
#' predIdx <- c(which(demoLGCM$ID==1),which(demoLGCM$ID==2)[1])
#' fitDemoLGCM <- demoLGCM[setdiff(1:nrow(demoLGCM),predIdx),]
#'
#' lgcm <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
#'              fitDemoLGCM,'ID','y')
#' lgcm <- fit(lgcm)
#' predRes <- predict(lgcm,demoLGCM[predIdx,])
#' }
#' @export
predict.GPPM <- function(object,newData,...){
validate_predict(object,newData)
checkFitted(object)

#get relevant old data
oldData <- datas(object)
IDfield <- getID(oldData)
predictionIDs <- unique(newData[,IDfield])
oldData <- oldData[oldData[,IDfield] %in% predictionIDs,]
dataUpdate <- rbind(oldData,newData)

#get model implied mean and covariance matrices for all relevant persons
newModel <- updateData(object,dataUpdate)
newModel <- fit(newModel,useOptimizer=FALSE,init=coef(object),...)
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
  res$preds[[i]] <- onlyDataForPersonI[indexNew,preds(object),drop=FALSE]
  res$trueVals[[i]] <- onlyDataForPersonI[indexNew,attr(oldData,'DV'),drop=FALSE]
  res$DV <- getDV(oldData)
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

#' Accuracy Estimates for Predictions
#'
#' Estimate the accuracy based on predictions.
#'
#' @param predRes  object of class \code{GPPMPred} as obtained by \code{\link{predict.GPPM}}
#' @return accuracy estimates in the form of the mean squared error (MSE), the negative log-predictive probability (nLPP), and the sum squared error (SSE)
#'
#' @examples
#' \dontrun{
#'data("demoLGCM")
#' #remove all measurements from person 1 and the first form person 2
#' predIdx <- c(which(demoLGCM$ID==1),which(demoLGCM$ID==2)[1])
#' fitDemoLGCM <- demoLGCM[setdiff(1:nrow(demoLGCM),predIdx),]
#'
#' lgcm <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
#'              fitDemoLGCM,'ID','y')
#' lgcm <- fit(lgcm)
#' predRes <- predict(lgcm,demoLGCM[predIdx,])
#' accEsts <- accuracy(predRes)
#' accEsts$MSE #mean squared error
#' accEsts$nLPP #negative log-predictive probability
#' }
#' @export
accuracy <- function(predRes){
  nPers <- length(predRes$ID)
  MSE <- c()
  LL <- rep(NA,nPers)
  for (i in 1:nPers){
    MSE <- c(MSE,((predRes$predMean[[i]]-predRes$trueVals[[i]])^2)[,1])
    LL[i] <- log(mvtnorm::dmvnorm(t(predRes$trueVals[[i]]),mean=as.vector(predRes$predMean[[i]]),sigma=predRes$predCov[[i]]))
  }
  mMSE <- mean(MSE)
  sLL <- sum(LL)
  sSE <- sum(MSE)
  return(list(MSE=mMSE,nLPP=-sLL,SSE=sSE))
}


#' Plotting predictions
#'
#' Plots person-specific predictions
#'
#' @param x object of class \code{GPPMPred} as obtained by \code{\link{predict.GPPM}}
#' @param plotId character string or integer. ID of the person for which the predictions should be plotted
#' @param ... additional arguments (currently not used).
#' @return A plot visualizing the predictive distribution. The bold line describes the mean and the shaded area the 95\% credibility interval.
#'
#' @examples
#' \dontrun{
#'data("demoLGCM")
#' #remove all measurements from person 1 and the first form person 2
#' predIdx <- c(which(demoLGCM$ID==1),which(demoLGCM$ID==2)[1])
#' fitDemoLGCM <- demoLGCM[setdiff(1:nrow(demoLGCM),predIdx),]
#'
#' lgcm <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
#'              fitDemoLGCM,'ID','y')
#' lgcm <- fit(lgcm)
#' predRes <- predict(lgcm,demoLGCM[predIdx,])
#' plot(predRes,1)
#' }
#' @method plot GPPMPred
#' @export
plot.GPPMPred <- function(x,plotId,...){
  stopifnot(length(plotId)==1)
  idIdx = plotId==x$ID
  stopifnot(sum(idIdx)==1) #only for one subject
  idIdx = which(idIdx) #solved indexing errors

  means <- x$predMean[[idIdx]]
  vars <- diag(x$predCov[[idIdx]])
  cv <- 1.96
  lb <- means+cv*vars
  ub <- means-cv*vars
  toPlot <- data.frame(mypreds=x$preds[[idIdx]],theMeans=means,lb=lb,ub=ub,trueV=x$trueVals[[idIdx]])
  thePlot <- ggplot(toPlot, aes_string(x=names(toPlot)[1], y='means'))+
  geom_line(aes_string(y='ub')) +
  geom_line(aes_string(y='lb')) +
  geom_point(aes_string(y='means')) +
  geom_ribbon(aes_string(ymax='ub', ymin='lb'), fill="grey", alpha=.5)+
  geom_line(size=1)

  thePlot <- thePlot + ggthemes::theme_tufte()+xlab(paste0('Predictor ',names(toPlot)[1]))+ylab(paste0('Outcome ', x$DV))
  thePlot
}





