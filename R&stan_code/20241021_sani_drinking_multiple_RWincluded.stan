//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> n;
  vector[n] coverage;
  vector[n] GDP;
  vector[n] di;
  vector[n] UP;
  vector[n] RW;
  vector[n] dummy;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real c1;
  real c2;
  real c3;
  real c4;
  real c5;
  real intercept;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  c1 ~ normal(-10, 10);
  c2 ~ normal(-10, 10);
  c3 ~ normal(-10, 10);
  c4 ~ normal(-10, 10);
  c5 ~ normal(-10, 10);
  vector[n] x1;
  vector[n] x2;
  
  for (i in 1:n){
  x1[i] = exp(intercept + c1*GDP[i] + c2*UP[i] + c3*di[i] + c4*RW[i] + c5*dummy[i]);
  x2[i] = x1[i] / (1 + x1[i]);
  coverage[i] ~ normal(x2[i], sigma);
}  
}
