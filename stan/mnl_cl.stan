data {
	int<lower=2> C;                   // # of alternatives
	int<lower=1> N;	                  // # of individuals
	int<lower=1> K;                   // # of covariates of invdividuals
	int<lower=1> KC;                  // # of covariates of alternatives
	vector<lower=0,upper=1>[C] Z[N];  // # index if alternative was considered 
	matrix[C, K] X[N];                // matrix of attributes for each individual
	matrix[C, KC] X2[N];              // matrix of attributes for each alternatives
	vector<lower = 0, upper = 1>[C] choice[N]; // binary indicator for final choice
}

parameters {
	vector[K] beta;       // individual attributes
	vector[KC] zeta;      // party attributes
}
model {
  matrix[C,N] xb;
  vector[C] utilities[N];
  vector[C] log_prob[N];
  
  // priors
	beta ~ normal(0, 5);  
	zeta ~ normal(0, 5);  

	// model
	for (i in 1:N){
	  xb[,i] = exp(X[i]*beta+X2[i]*zeta); // individual + alternative specific
	  utilities[i] = xb[,i] .* Z[i];      // set alternatives that were not considered to 0
	  
	  log_prob[i] = log(softmax(utilities[i]));
    target += log_prob[i] * choice[i]';
    
	  }
}
