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

/* Stan program I.3 */

data {
  int<lower=1> nd; // number of data elements
  int<lower=1> ns; // number of subjects
  int<lower=2> nt; // number of treatments
  int<lower=1, upper=ns> subj[nd]; // subject indicator
  int<lower=1, upper=nt> treat[nd]; // treatment indicator
  vector[nd] outcome; // outcome vector
}

transformed data {
  vector[nt] THETA = rep_vector(0, nt); // MU prior mean
  matrix[nt,nt] ETA = diag_matrix( rep_vector(1e6, nt) ); // MU prior covariance
  matrix[nt,nt] DELTA = diag_matrix( rep_vector(10, nt) ); // PSI prior matrix
}

parameters {
  vector[nt] MU; // fixed effects
  cov_matrix[nt] PSI; // random effects cov_matrix
  vector[nt] ALPHA[ns]; // random effects
  real<lower=0> sigma; // residual variance
}

/*
stan_plot( object, pars=c("PTE", "ITE") );
Comment block if you do not require this plot.
*/
transformed parameters {
  vector[nt-1] PTE; // population treatment effects
  vector[ns] ITE[nt-1]; // individual treatment effects
  for (t in 2:nt) { // loop through the treatments
    PTE[t-1] = MU[t];
    for (s in 1:ns) { // loop through the subjects
      ITE[t-1][s] = ALPHA[s][t];
    }
  }
}

model {
  real average;
  MU ~ multi_normal(THETA, ETA); // hyperprior
  PSI ~ inv_wishart(nt, DELTA); // hyperprior
  ALPHA ~ multi_normal(MU, PSI); // prior
  sigma ~ inv_gamma(0.01, 10); // prior
  for (i in 1:nd) { // loop through the observations
    average = ALPHA[subj[i]][1]; // add random intercept
    for (t in 2:nt) average += ALPHA[subj[i]][t]*(treat[i]==t); // add treatment effects
    outcome[i] ~ normal( average, sqrt(sigma) ); // likelihood
  }
}
