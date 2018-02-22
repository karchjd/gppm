convertFromWide <- function(myData){
    res <- as.data.frame(matrix(NA,nrow=nrow(myData)*ncol(myData)/2,ncol=3))
    names(res) <- c('ID','t','Y')
    counter <- 1
    for (i in 1:nrow(myData)){
      for(j in 1:10){
      res[counter,'ID'] <- i
      res[counter,'t'] <- myData[i,paste0('t',j)]
      res[counter,'Y'] <- myData[i,paste0('Y',j)]
      counter <- counter + 1
      }
    }
    res
}

require(gppm)
require(OpenMx)
require(MASS)
  ##settings
  nP <- 30
  nT <- 10

  ##define latent growth curve model using SEM software
  lgcModel <- generateLGCM(nT)
  trueModel <- omxSetParameters(lgcModel,labels=c('muI','muS','varI','varS','covIS','sigma'),values=c(10,3,4,10,0.5,10))

  ##simulate data
  yMatrix <- gppm::simulateData(trueModel,N=nP)
  tMatrix <- matrix(rep(0:(nT-1),each=nP),nrow = nP,ncol = nT)
  colnames(tMatrix) <- paste0('t',1:nT)
  myData <- as.data.frame(cbind(tMatrix,yMatrix))

  ##fit data using SEM
  # semModel <- mxModel(lgcModel,mxData(yMatrix,type = "raw"))
  # semModel <- mxRun(semModel,silent = TRUE)



  ##fit data using GPPMNew
  longData <- convertFromWide(myData)
  longData$IQ <- 0
  longDataTrain <- longData[1:295,]
  longDataTest <- longData[296:nrow(longData),]
  gpModel <- gppModel('muI+IQ+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',longDataTrain,ID='ID',DV='Y')
  fittedModel <- fit(gpModel)
  predict(fittedModel,longDataTest)



  ##compare results
  # lgcmSame <- all.equal(gpModelFit$mlParas,omxGetParameters(semModel)[names(gpModelFit$mlParas)],check.attributes=FALSE,tolerance=0.0001)
  # message(sprintf('Estimated parameters for the LGCM model are the same: %s',lgcmSame))
