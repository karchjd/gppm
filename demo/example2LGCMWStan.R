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

extractStuff <- function(stanRes,name,index){
  P <- stanRes$data$P[index]
  switch(name,
          Sigma={
            res <- matrix(nrow=P,ncol=P)
            for (i in 1:P){
              for (j in 1:P){

              }
            }
          },
         {
           error()
         }
  )
  return(res)
}
 require(gppmr)
 require(OpenMx)
 require(MASS)
 require(rstan)
 require(tictoc)
   ##settings
   nP <- 40
   nT <- 15

   trueVarI <- 4
   ##define latent growth curve model using SEM software
   lgcModel <- generateLGCM(nT)
   trueModel <- omxSetParameters(lgcModel,labels=c('muI','muS','varI','varS','covIS','sigma'),values=c(10,3,trueVarI,10,0.5,10))

   ##simulate data
   nReps <- 100
   stanIn <- rep(NA,nReps)
   mxIn <- stanIn

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
     gpModel <- gppModelOld('muI+muS*t','varI+covIS*(t+t!)+varS*t*t!+omxApproxEquals(t,t!,0.0001)*sigma',myData)
     tic()
     # gpModelFit <- gppFit(gpModel)
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
     stanRes <- optimizing(theModel,dataForStan,hessian = TRUE)
     stanCov <- solve(-stanRes$hessian)
     stanRes$data <- dataForStan
     toc()
     ##compare results
     # lgcmSame <- all.equal(gpModelFit$mlParas,omxGetParameters(semModel)[names(gpModelFit$mlParas)],check.attributes=FALSE,tolerance=0.0001)
     lgcmSame <- TRUE
     omxParas <- omxGetParameters(semModel)
     lgcmSame2 <- all.equal(omxParas,stanRes$par[names(omxParas)],check.attributes=FALSE,tolerance=0.01)
     message(sprintf('Estimated parameters for the LGCM model are the same: %s',lgcmSame && lgcmSame2))

     #variances

      mxCov <- vcov(semModel)[rownames(sampCov),colnames(sampCov)]
      stan196 <- 1.96*sqrt(stanCov['varI','varI'])
      mx196 <- 1.96*sqrt(mxCov['varI','varI'])
      mxML <- omxGetParameters(semModel)['varI']
      stanML <- stanRes$par['varI']
#
      #is true parameter in confidence intervals?
      stanIn[j] <- trueVarI > (stanML-stan196) & trueVarI < (stanML+stan196)
      mxIn[j] <- trueVarI > (mxML-mx196) & trueVarI < (mxML+mx196)


