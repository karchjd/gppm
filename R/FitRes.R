new_FitRes <- function(paraEsts,vcov,minus2LL,nPar,mu,Sigma){
  stopifnot(is.double(paraEsts)) #maximum likelihood estimates
  stopifnot(is.matrix(vcov)) #covariance of sampling distribution of maximum likelihood estimator
  stopifnot(is.double(minus2LL) && length(minus2LL)==1)
  stopifnot(is.integer(nPar) && length(nPar)==1)
  stopifnot(is.list(mu))
  stopifnot(is.list(Sigma))

  structure(list(
    paraEsts=paraEsts,
    vcov=vcov,
    minus2LL=minus2LL,
    nPar=nPar,
    mu=mu,
    Sigma=Sigma
  ),
  class='StanData'
  )
}

extractMoments <- function(stanOutParas,dataStats){
  mu <- rep(list(numeric(dataStats$maxTime)),dataStats$nPer)
  Sigma <- rep(list(matrix(nrow=dataStats$maxTime,ncol = dataStats$maxTime)),dataStats$nPer)
  for (iPer in 1:dataStats$nPer){
    for (iTime1 in 1:dataStats$nTime[iPer]){
      mu[[iPer]][iTime1] <- stanOutParas[sprintf('mu[%i,%i]',iPer,iTime1)]
      for (iTime2 in 1:dataStats$nTime[iPer]){
        Sigma[[iPer]][iTime1,iTime2] <- stanOutParas[sprintf('Sigma[%i,%i,%i]',iPer,iTime1,iTime2)]
      }
    }
    mu[[iPer]] <- mu[[iPer]][1:dataStats$nTime[iPer]]
    Sigma[[iPer]] <- Sigma[[iPer]][1:dataStats$nTime[iPer],1:dataStats$nTime[iPer]]
  }
  res <- list(mu=mu,Sigma=Sigma,IDs=attr(dataStats,'IDs'))
}

extractFitRes <- function(stanOut,parsedModel,dataStats){
  paraEsts <- stanOut$par[parsedModel$params]
  vcov <- solve(-stanOut$hessian)

  minus2LL <- stanOut$value
  nPar <- length(parsedModel$params)
  meanCov <- extractMoments(stanOut$par,dataStats)
  mu=meanCov$mu
  Sigma=meanCov$Sigma
  new_FitRes(paraEsts,vcov,minus2LL,nPar,mu,Sigma)
}
