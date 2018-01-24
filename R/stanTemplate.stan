data{
  int<lower=1> nPer;
  int<lower=1> nTime[nPer];
  int<lower=1> maxTime;
  int<lower=1> nPreds;
  matrix[maxTime,nPreds] X[nPer];
  matrix[nPer,maxTime] Y;
}

parameters{
  real paras;
}

transformed parameters{
  matrix[nPer,maxTime] mu;
  matrix[maxTime,maxTime] Sigma[nPer];
  matrix[maxTime,maxTime] cholSigma[nPer];
  for (i in 1:nPer){
    mu[i] = <meanfunction>;
    for(j in 1:nTime[i]){
      for(k in 1:nTime[i]){
        Sigma[i,j,k] = <covfunction>;
      }
    }
    L[i,1:nTime[i],1:nTime[i]] = cholesky_decompose(Sigma[i,1:nTime[i],1:nTime[i]]);
  }
}

model{
  for (i in 1:nPer){
    Y[i,1:nTime[i]] ~  multi_normal_cholesky(mu[i,1:nTime[i]], L[i,1:nTime[i],1:nTime[i]]);
  }
}
