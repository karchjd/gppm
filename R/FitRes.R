new_FitRes <- function(paraEsts,vcov,minus2LL,nPar,AIC,BIC,mu,Sigma){
  stopifnot(is.double(paraEsts)) #maximum likelihood estimates
  stopifnot(is.matrix(vcov)) #covariance of sampling distribution of maximum likelihood estimator
  stopifnot(is.double(minus2LL) && length(minus2LL)==1)
  stopifnot(is.integer(nPar) && length(nPar)==1)
  stopifnot(is.double(AIC) && length(AIC)==1)
  stopifnot(is.double(BIC) && length(BIC)==1)
  stopifnot(is.list(mu))
  stopifnot(is.list(Sigma))

  structure(list(
    paraEsts=paraEsts,
    vcov=vcov,
    minus2LL=minus2LL,
    nPar=nPar,
    AIC=AIC,
    BIC=BIC,
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
    for (iTime1 in 1:dataStats$nTime[i]){
      mu[[i]][iTime1] <- stanRes$par[sprintf('mu[%i,%i]',iPer,iTime1)]
      for (iTime2 in 1:dataStats$nTime[i]){
        Sigma[[i]][iTime1,iTime2] <- stanRes$par[sprintf('Sigma[%i,%i,%i]',iPer,iTime1,iTime2)]
      }
    }
    mu[[i]] <- mu[[i]][1:dataStats$nTime[i]]
    Sigma[[i]] <- Sigma[[i]][1:dataStats$nTime[i],1:dataStats$nTime[i]]
  }
  list(mu=mu,Sigma=Sigma)
}

extractFitRes <- function(stanOut,parsedModel,dataStats){
  paraEsts <- stanOut$par[parsedModel$params]
  vcov <- solve(-stanOut$hessian)

  minus2LL <- stanOut$value
  nPar <- length(parsedModel$params)
  AIC <- minus2LL+nPar
  BIC <- minus2LL+log(dataStats$nPer)*nPar
  meanCov <- extractMoments(stanOut$params,dataStats)
  mu=meanCov$mu
  Sigma=meanCov$Sigma
  new_FitRes(paraEsts,vcov,minus2LL,nPar,AIC,BIC,mu,Sigma)
}
