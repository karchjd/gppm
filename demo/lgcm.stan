data{
  int<lower=1> N;
  int<lower=1> P[N];
  int<lower=1> maxP;
  matrix[N,maxP] X;
  matrix[N,maxP] Y;
}

parameters{
  real muI;
  real muS;
  real varI;
  real varS;
  real covIS;
  real sigma;
}

transformed parameters{
  matrix[N,maxP] mu;
  matrix[maxP,maxP] Sigma[N];
  matrix[maxP,maxP] L[N];
  for (i in 1:N){
    mu[i] = muI+muS*X[i];
    for(j in 1:P[i]){
      for(k in 1:P[i]){
        Sigma[i,j,k] = varI+covIS*(X[i,j]+X[i,k])+varS*X[i,j]*X[i,k]+(j==k)*sigma;
      }
    }
    L[i,1:P[i],1:P[i]] = cholesky_decompose(Sigma[i,1:P[i],1:P[i]]);
  }
}

model{
  for (i in 1:N){
    Y[i,1:P[i]] ~  multi_normal_cholesky(mu[i,1:P[i]], L[i,1:P[i],1:P[i]]);
  }
}


