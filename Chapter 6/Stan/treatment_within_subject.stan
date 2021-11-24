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

/* Stan program I.1 */

data {
  int<lower=1> nd; // number of data elements
  int<lower=1> ns; // number of subjects
  int<lower=2> nt; // number of treatments
  int<lower=2> nm; // number of subject*treatment levels
  int<lower=1> nc; // number of subject*cycle levels
  int<lower=1, upper=ns> subj[nd]; // subject indicator
  int<lower=1, upper=nt> treat[nd]; // treatment indicator
  int<lower=1, upper=nm> subj_treat[nd]; // subject*treatment indicator
  int<lower=1, upper=nc> subj_cycle[nd]; // subject*cycle indicator
  vector[nd] outcome; // outcome vector
}

parameters {
  real mu; // intercept
  real pte[nt-1]; // treatment coefficients
  real<lower=0> phi; // between subject variance
  real<lower=0> psi; // subject*treatment variance
  real<lower=0> eta; // subject*cycle variance
  real<lower=0> sigma; // residual variance
  vector[ns] ALPHA; // subject random effects
  vector[nm] THETA; // subject*treatment random effects
  vector[nc] OMEGA; // subject*cycle random effects
}

transformed parameters {
  vector[ns] ITE[nt-1]; // individual treatment effects
  for (s in 1:ns) { // loop through the subjects
    for (t in 2:nt) { // loop through the treatments
      ITE[t-1][s] = pte[t-1]+THETA[ns*(t-1)+s]-THETA[s];
    }
  }
}

model {
  real average;
  mu ~ normal(0, 1e6); // prior
  pte ~ normal(0, 1e6); // prior
  phi ~ inv_gamma(0.01, 10); // hyperprior
  psi ~ inv_gamma(0.01, 10); // hyperprior
  eta ~ inv_gamma(0.01, 10); // hyperprior
  sigma ~ inv_gamma(0.01, 10); // prior
  ALPHA ~ normal( 0, sqrt(phi) ); // subject random effects
  THETA ~ normal( 0, sqrt(psi) ); // subject*treatment random effects
  OMEGA ~ normal( 0, sqrt(eta) ); // subject*cycle random effects
  for (i in 1:nd) { // loop through the observations
    average = mu+ALPHA[subj[i]]; // add intercept plus subject random effect
    for (t in 2:nt) average += pte[t-1]*(treat[i]==t); // add treatment fixed effects
    average += THETA[subj_treat[i]]; // add subject*treatment interaction
    average += OMEGA[subj_cycle[i]]; // add subject*cycle interaction
    outcome[i] ~ normal( average, sqrt(sigma) ); // likelihood
  }
}
