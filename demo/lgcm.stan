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
real<lower=0> varI;
real<lower=0> varS;
real covIS;
real<lower=0> sigma;

}

model{
matrix[N,maxP] mu;
matrix[maxP,maxP] Sigma[N];
for (i in 1:N){
    mu[i] = muI+muS*X[i];
  for(j in 1:P[i]){
    for(k in 1:P[i]){
      Sigma[i,j,k] = varI+covIS*(X[i,j]+X[i,k])+varS*X[i,j]*X[i,k];
    }
  }
  
  for(k in 1:P[i]){
    Sigma[i,k,k] = Sigma[i,k,k] + sigma;
  }

  Y[i,1:P[i]] ~ multi_normal(mu[i,1:P[i]], Sigma[i][1:P[i],1:P[i]]);
}
}
