data {
  int<lower=0> Nobs;
  int<lower=0> Npreds;
  int<lower=0> Nlevel1;
  vector[Nobs] response;
  matrix[Nobs, Npreds] X;
  int<lower=1, upper=Nlevel1> levind1[Nobs];
  real<lower=0> sdscal;
}
parameters {
  vector[Npreds] beta;
  real<lower=0> sigmalev1;
  real<lower=0> sigmaepsilon;
  vector[Nlevel1] eta1;
}
transformed parameters {
  vector[Nlevel1] ran1; 
  vector[Nobs] yhat;

  ran1 <- sigmalev1 * eta1;
  
  for (i in 1:Nobs){
    yhat[i] <- X[i] * beta + ran1[levind1[i]];  }
}
model {
  eta1 ~ normal(0, 1);
  sigmalev1 ~ cauchy(0, 2.5*sdscal);
  sigmaepsilon ~ cauchy(0, 2.5*sdscal);
  response ~ normal(yhat, sigmaepsilon);
}



