require(gppmr)
require(OpenMx)
require(MASS)
  ##settings
  nP <- 300
  nT <- 50

  ##define latent growth curve model using SEM software
  lgcModel <- generateLGCM(nT)
  trueModel <- omxSetParameters(lgcModel,labels=c('muI','muS','varI','varS','covIS','sigma'),values=c(10,3,4,10,0.5,10))

  ##simulate data
  yMatrix <- simulateData(trueModel,N=nP)
  tMatrix <- matrix(rep(0:(nT-1),each=nP),nrow = nP,ncol = nT)


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
  # gpModelFit <- gppFit(gpModel)
  toc()
  #fit data using stan
  theModel <- stan_model(file = 'lgcm.stan')
  dataForStan <- list(X=tMatrix,Y=yMatrix,N=nrow(yMatrix),P=ncol(yMatrix))
  tic()
  stanRes <- optimizing(theModel,dataForStan)  
  toc()
  
  ##compare results
  # lgcmSame <- all.equal(gpModelFit$mlParas,omxGetParameters(semModel)[names(gpModelFit$mlParas)],check.attributes=FALSE,tolerance=0.0001)
  lgcmSame2 <- all.equal(omxGetParameters(semModel)[names(gpModelFit$mlParas)],stanRes$par[names(gpModelFit$mlParas)],check.attributes=FALSE,tolerance=0.0001)
  message(sprintf('Estimated parameters for the LGCM model are the same: %s',lgcmSame && lgcmSame2))
