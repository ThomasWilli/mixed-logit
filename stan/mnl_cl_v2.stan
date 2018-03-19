data {
  int<lower=1> N;                   // # of individuals
  int<lower=1> K;                   // # of predictors
  int<lower=2> J;                   // number of available alternatives
  int<lower=1, upper=J> y[N];       // outcome of chosen alternative
  matrix[N*J, K] x;                 // predictor matrix
  vector<lower=0,upper=1>[J] Z[N];  // # index if alternative was considered 
}
parameters {
  vector[K] beta;
}
model {
  vector[J] utilities[N];
  beta ~ normal(0, 5);

  for (n in 1:N) {
    utilities[n] = x[((n-1)*J+1):(n*J),] * beta;
    utilities[n] = utilities[n] .* Z[n];
    y[n] ~ categorical(softmax(utilities[n]));
  }
}
