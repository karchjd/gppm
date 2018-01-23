padLists <- function(Xlist,Ylist){
  fakeD <- 10^100
  numberPersons <- length(Ylist)
  measuresPerPerson <- rep(NA,numberPersons)
  for (i in 1:numberPersons){
    measuresPerPerson[i] <- length(Ylist[[i]])
  }
  maxMeasures <- max(measuresPerPerson)
  for (i in 1:numberPersons){
    Xlist[[i]] <- c(Xlist[[i]],rep(fakeD,maxMeasures-length(Xlist[[i]])))
    Ylist[[i]] <- c(Ylist[[i]],rep(fakeD,maxMeasures-length(Ylist[[i]])))
  }
    res <- list(measuresPerPerson=measuresPerPerson)
  return(list(Xlist=Xlist,Ylist=Ylist,measuresPerPerson=measuresPerPerson))
}

require(gppmr)
require(OpenMx)
require(MASS)
require(rstan)
require(tictoc)
  ##settings
  nP <- 300
  nT <- 20

  ##define latent growth curve model using SEM software
  lgcModel <- generateLGCM(nT)
  trueModel <- omxSetParameters(lgcModel,labels=c('muI','muS','varI','varS','covIS','sigma'),values=c(10,3,4,10,0.5,10))

  ##simulate data
  yMatrix <- simulateData(trueModel,N=nP)
  tMatrix <- matrix(rep(0:(nT-1),each=nP),nrow = nP,ncol = nT)
  yMatrix[rbinom(prod(dim(yMatrix)),1,0.2)] <- NA


  ##fit data using SEM
  semModel <- mxModel(lgcModel,mxData(yMatrix,type = "raw"))
  tic()
  semModel <- mxRun(semModel,silent = TRUE)
  toc()
  colnames(tMatrix) <- paste0('t',1:nT)
  myData <- as.data.frame(cbind(tMatrix,yMatrix))

  # ##fit data using GPPM
  gpModel <- gppModel('muI+muS*t','varI+covIS*(t+t!)+varS*t*t!+omxApproxEquals(t,t!,0.0001)*sigma',myData)
  tic()
  gpModelFit <- gppFit(gpModel)
  toc()
  #fit data using stan
  theModel <- stan_model(file = 'lgcm.stan')
  Ylist <- list()
  Xlist <- list()
  for (i in 1:nrow(yMatrix)){
    notIsNa <- !is.na(yMatrix[i,])
    Ylist[[i]] <- yMatrix[i,notIsNa]
    Xlist[[i]] <- tMatrix[i,notIsNa]
  }
  newLists <- padLists(Xlist,Ylist)
  dataForStan <- list(X=newLists$Xlist,Y=newLists$Ylist,N=nrow(yMatrix),P=newLists$measuresPerPerson,maxP=max(newLists$measuresPerPerson))
  tic()
  stanRes <- optimizing(theModel,dataForStan)  
  toc()
  
  ##compare results
  lgcmSame <- all.equal(gpModelFit$mlParas,omxGetParameters(semModel)[names(gpModelFit$mlParas)],check.attributes=FALSE,tolerance=0.0001)
  lgcmSame2 <- all.equal(omxGetParameters(semModel)[names(gpModelFit$mlParas)],stanRes$par[names(gpModelFit$mlParas)],check.attributes=FALSE,tolerance=0.0001)
  message(sprintf('Estimated parameters for the LGCM model are the same: %s',lgcmSame && lgcmSame2))
