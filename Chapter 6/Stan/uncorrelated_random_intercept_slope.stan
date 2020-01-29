/*
Written by Artur Araujo
November 2019

artur.stat@gmail.com

while working
on the IDEAL project

http://www.ideal.rwth-aachen.de/
*/

/* Stan program I.5 */

data {
  int<lower=1> nd; // number of data elements
  int<lower=1> ns; // number of subjects
  int<lower=2> nt; // number of treatments
  int<lower=1, upper=ns> subj[nd]; // subject indicator
  int<lower=1, upper=nt> treat[nd]; // treatment indicator
  vector[nd] outcome; // outcome vector
}

parameters {
  real mu; // intercept
  real pte[nt-1]; // treatment coefficients
  real<lower=0> phi; // random intercept variance
  real<lower=0> psi[nt-1]; // random slope variance vector
  vector[ns] ALPHA; // random intercept
  vector[ns] ITE[nt-1]; // random slope
  real<lower=0> sigma; // residual variance
}

model {
  real average;
  mu ~ normal(0, 1e4); // hyperprior
  pte ~ normal(0, 1e4); // hyperprior
  phi ~ inv_gamma(0.01, 0.01); // hyperprior
  psi ~ inv_gamma(0.01, 0.01); // hyperprior
  ALPHA ~ normal( mu, sqrt(phi) ); // prior
  for ( t in 1:(nt-1) ) ITE[t] ~ normal(pte[t], sqrt( psi[t]) ); // prior
  sigma ~ inv_gamma(0.01, 0.01); // prior
  for (i in 1:nd) { // loop through the observations
    average = ALPHA[subj[i]]; // add random intercept
    for (t in 2:nt) average += ITE[t-1][subj[i]]*(treat[i]==t); // add treatment effects
    outcome[i] ~ normal( average, sqrt(sigma) ); // likelihood
  }
}
