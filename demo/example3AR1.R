#generate a Latent Growth Curve Model
generateAR <-function(N,measureError=FALSE){
  ##define model
  maniFests <- paste('x',1:N,sep="")
  latents <- paste('y',1:N,sep="")
  model<-mxModel("Autoregressive Model Path",
                 type="RAM",
                 manifestVars=maniFests,
                 latentVars=c(latents),
                 mxPath(from=latents[1:(N-1)],
                        to=latents[2:N],
                        arrows=1,
                        free=TRUE,
                        values=rep(.8,N-1),
                        labels=rep("b1",N-1)
                 ),
                 mxPath(from=latents,
                        arrows=2,
                        free=TRUE,
                        values=rep(.1,N),
                        labels=rep('sigma',N),
                        lbound=0),
                 mxAlgebra(name="cstrV", expression=sigma/(1-b1^2)),
                 mxPath(from=latents[1],arrows=2,free=F,labels="cstrV[1,1]"),
                 mxPath(from=latents,
                        to=maniFests,
                        arrows=1,
                        free=FALSE,
                        values=rep(1,N)
                 ),
                 mxPath(from="one",
                        to=latents[2:N],
                        arrows=1,
                        free=T,
                        values=rep(1,N-1),
                        labels=rep('b0',N-1)
                 ),
                 mxAlgebra(name="cstrM", expression=b0/(1-b1)),
                 mxPath(from="one",
                        to=latents[1],
                        arrows=1,
                        free=FALSE,
                        labels="cstrM[1,1]"
                 )
  ) # close model
  if (measureError){
    warning('Untested!!!')
    model <-mxModel(model,mxPath(from=maniFests,
                                 arrows=2,
                                 free=TRUE,
                                 values=rep(.1,N),
                                 labels=rep('noise',N),
                                 lbound=0,
    ))
  }
  return(model)
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

library(gppm)
library(OpenMx)

##settings
nT <- 100 #number of time points

##define autoregressive model using SEM software
arModel <- generateAR(nT)
trueModel <- omxSetParameters(arModel,labels = c('b0','b1','sigma'),values = c(10,0.5,1))

##simulate data
yVector <- simulateData(trueModel)
tVector <- 1:nT

##fit data using SEM
semModel <- mxModel(arModel,mxData(yVector,type = "raw"))
semModel <- mxRun(semModel)


#get results using GPPM
myData <- data.frame(ID=rep(1,nT),t=tVector,y=as.numeric(yVector))
gpModel <- gppm('b0/(1-b1)','b1^(fabs(t-t#))*sigma/(1-b1^2)',myData,ID='ID',DV='y')
gpModel <- fit(gpModel)

##check results
arSame <- all.equal(coef(gpModel),omxGetParameters(semModel)[names(coef(gpModel))],check.attributes=FALSE,tolerance=0.0001)
message(sprintf('Estimated parameters for the AR(1) model are the same: %s',arSame))
