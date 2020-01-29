/*
Written by Artur Araujo
June 2019

Modified by Artur Araujo
November 2019

artur.stat@gmail.com

while working
on the IDEAL project

http://www.ideal.rwth-aachen.de/
*/

/* Stan program I.7 */

data {
  int<lower=1> nd; // number of data elements
  int<lower=1> ns; // number of subjects
  int<lower=1, upper=ns> subj[nd]; // subject indicator
  vector[nd] outcome; // outcome vector
}

parameters {
  real pte; // intercept
  real<lower=0> psi; // between subject variance
  vector[ns] ITE; // random effects
  real<lower=0> sigma; // residual variance
}

model {
  pte ~ normal(0, 1e4); // hyperprior
  psi ~ inv_gamma(0.01, 0.01); // hyperprior
  ITE ~ normal( pte, sqrt(psi) ); // subject random effects
  sigma ~ inv_gamma(0.01, 0.01); // prior
  for (i in 1:nd) {
    outcome[i] ~ normal( ITE[subj[i]], sqrt(sigma) ); // likelihood
  }
}
