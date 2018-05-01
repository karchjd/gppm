convertFromWide <- function(myData){
    res <- as.data.frame(matrix(NA,nrow=nrow(myData)*ncol(myData)/2,ncol=3))
    names(res) <- c('ID','t','Y')
    counter <- 1
    for (i in 1:nrow(myData)){
      for(j in 1:10){
      res[counter,'ID'] <- i
      res[counter,'t'] <- myData[i,paste0('t',j)]
      res[counter,'Y'] <- myData[i,paste0('x',j)]
      counter <- counter + 1
      }
    }
    res
}

#simulate Data for on OpenMx Model
simulateData <- function(run,N=NA) {
  fit <- NULL
  data <- NULL
  manifests <- run@manifestVars
  p <- length(run@manifestVars)
  defVar <- !is.null(run$data)
  if (defVar){
    N=run$data$numObs
  }
  if (is.na(N)){
    N <-1
  }
  #fake data for manifest observations
  fake.data <- data.frame(matrix(1:(length(manifests)*N),ncol=length(manifests)))

  #add fake data
  if (defVar){
    run$data@observed[,manifests] <- fake.data
  }else{
    colnames(fake.data) <- manifests
    run <- mxModel(run,mxData(fake.data,type="raw"))
  }

  #run to get exp mean and covariance
  fit <- mxRun(run,useOptimizer=F,silent=T)
  covM <- fit$fitfunction@info$expCov
  mu <- fit$fitfunction@info$expMean

  #generate Data
  simData <- mvrnorm(n=N, mu, covM)
  if (N==1){
    simData <- t(simData)
  }
  colnames(simData) <- manifests

  #propagate defintion variables
  if (defVar){
    tmp <- run$data@observed
    tmp[,manifests]  <- simData
    simData <- tmp
  }
  return(simData)
}

#generate a Latent Growth Curve Model
generateLGCM <-function(p,N=0,posvar=FALSE){
  manifests <- paste('x',1:p,sep="")
  latents <-c("intercept","slope")
  if (posvar){
    lboundVar <- 0
  }else{
    lboundVar <- NA
  }
  if (N!=0){
    labTime <- paste("data.times",1:p,sep="")
  } else{
    labTime <- NA
  }

  growthCurveModel <- mxModel("Linear Growth Curve growthCurveModel Path Specification",
                              type="RAM",
                              manifestVars=manifests,
                              latentVars=latents,
                              # residual variances
                              mxPath(
                                from=manifests,
                                arrows=2,
                              free=TRUE,
                                values = 1,
                                labels= rep('sigma',p),
                              ),
                              # latent variances and covariance
                              mxPath(
                                from=latents,
                                arrows=2,
                                free=TRUE,
                                connect="unique.pairs",
                                values=c(1,0.5,1),
                                labels=c("varI",'covIS',"varS"),
                                lbound = c(lboundVar,NA,lboundVar)
                              ),
                              # intercept loadings
                              mxPath(
                                from="intercept",
                                to=manifests,
                                arrows=1,
                                free=FALSE,
                                values=rep(1,p)
                              ),
                              # latent means
                              mxPath(
                                from="one",
                                to=latents,
                                arrows=1,
                                free=c(TRUE,TRUE),
                                values=c(1,0),
                                labels=c("muI",'muS')
                              ),
                              mxPath(
                                from="slope",
                                to=manifests,
                                arrows=1,
                                free=FALSE,
                                values=0:(p-1),
                                labels=labTime)
  ) # close growthCurveModel
  if (N!=0){
    ###create definition vars
    definitionVars <- repmat(t(0:(p-1)),N,1) / (p-1)

    ##add noise
    intervall <- definitionVars[1,2]-definitionVars[1,1]
    #add wave specific noise 20% to one side of the intra interval
    waveError <- intervall*0.2
    definitionVars <-definitionVars +  matrix(rep(runif(p,min=0,max=waveError),N),nrow=N,ncol=p,byrow=TRUE)
    #subject specific noise 10% of intra interval
    subjectError <- intervall*0.1
    numberdefinitionVars <- prod(dim(definitionVars))
    definitionVars <- definitionVars + matrix(runif(numberdefinitionVars,min=-subjectError,max=subjectError),dim(definitionVars))

    #clue definition variables to growthCurveModel
    emptyData <- data.frame(matrix(data=NA,nrow=N,ncol=p))
    fakeData <- cbind(emptyData,definitionVars)
    colnames(fakeData) <-  c(manifests,paste("times",1:p,sep=""))
    growthCurveModel <- mxModel(growthCurveModel,mxData(fakeData,type="raw"))
  }
  return(growthCurveModel)
}

require(gppm)
require(OpenMx)
require(MASS)
  ##settings
  nP <- 500
  nT <- 10

  ##define latent growth curve model using SEM software
  lgcModel <- generateLGCM(nT)
  trueModel <- omxSetParameters(lgcModel,labels=c('muI','muS','varI','varS','covIS','sigma'),values=c(10,3,4,10,0.5,10))

  ##simulate data
  yMatrix <- simulateData(trueModel,N=nP)
  tMatrix <- matrix(rep(0:(nT-1),each=nP),nrow = nP,ncol = nT)
  colnames(tMatrix) <- paste0('t',1:nT)
  myData <- as.data.frame(cbind(tMatrix,yMatrix))

  #fit data using SEM
  semModel <- mxModel(lgcModel,mxData(yMatrix,type = "raw"))
  semModel <- mxRun(semModel,silent = TRUE)



  ##fit data using GPPMNew
  longData <- convertFromWide(myData)
  gpModel <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',longData,ID='ID',DV='Y')
  fittedModel <- fit(gpModel,omxGetParameters(semModel),useOptimizer=FALSE)


  ##compare results
  lgcmSame <- all.equal(coef(fittedModel),omxGetParameters(semModel)[names(coef(fittedModel))],check.attributes=FALSE,tolerance=0.0001)
  message(sprintf('Estimated parameters for the LGCM model are the same: %s',lgcmSame))
