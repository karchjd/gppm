data{
int<lower=1> N;
int<lower=1> P;
matrix[N,P] X;
matrix[N,P] Y;
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
matrix[N,P] mu;
matrix[P,P] Sigma[N];
for (i in 1:N){
  for (j in 1:P){
    mu[i,j] = muI+muS*X[i,j];
  }
  for(j in 1:P){
    for(k in 1:P){
      Sigma[i,j,k] = varI+covIS*(X[i,j]+X[i,k])+varS*X[i,j]*X[i,k];
    }
  }
  
  for(k in 1:P){
    Sigma[i,k,k] = Sigma[i,k,k] + sigma;
  }

  Y[i] ~ multi_normal(mu[i], Sigma[i]);
}
}
