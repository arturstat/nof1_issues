/*
Written by Artur Araujo
November 2019

artur.stat@gmail.com

while working
on the IDEAL project

http://www.ideal.rwth-aachen.de/
*/

/* Stan program I.6 */

data {
  int<lower=3> nd; // number of data elements
  int<lower=1> ns; // number of subjects
  int<lower=2> nt; // number of treatments
  int<lower=1> np; // maximum number of periods
  int<lower=1, upper=ns> subj[nd]; // subject indicator
  int<lower=1, upper=nt> treat[nd]; // treatment indicator
  int<lower=1, upper=np> period[nd]; // period indicator
  vector[nd] outcome; // outcome vector
}

transformed data {
  int index[nd] = sort_indices_asc(subj); // sorted subject index
  int first[ns]; // index of first observation
  int last[ns]; // index of last observation
  int nmax = 1; // maximum size of individual information
  first[subj[index[1]]] = 1; // initialize array
  if (subj[index[1]] != subj[index[2]]) last[subj[index[1]]] = 1; // initialize array
  for ( i in 2:(nd-1) ) { // loop through the observations
    if (subj[index[i]] != subj[index[i-1]]) first[subj[index[i]]] = i; // first index
    if (subj[index[i]] != subj[index[i+1]]) last[subj[index[i]]] = i; // last index
  }
  if (subj[index[nd]] != subj[index[nd-1]]) first[subj[index[nd]]] = nd; // finalize array
  last[subj[index[nd]]] = nd; // finalize array
  for (s in 1:ns) nmax = max(last[s]-first[s]+1, nmax); // compute maximum size
}

parameters {
  real mu; // intercept
  real pte[nt-1]; // treatment coefficients
  real<lower=0> phi; // random intercept variance
  real<lower=0> psi[nt-1]; // random slope variance vector
  vector[ns] ALPHA; // random intercept
  vector[ns] ITE[nt-1]; // random slope
  real<lower=0> sigma; // residual variance
  real<lower=-1, upper=1> rho; // correlation coefficient
}

model {
  vector[nmax] outsubj; // individual outcome
  vector[nmax] average; // individual mean
  matrix[nmax,nmax] COV; // individual covariance matrix
  int irow; // row index
  int icol; // column index
  int j; // while loop counter
  mu ~ normal(0, 1e6); // hyperprior
  pte ~ normal(0, 1e6); // hyperprior
  phi ~ inv_gamma(0.01, 10); // hyperprior
  psi ~ inv_gamma(0.01, 10); // hyperprior
  ALPHA ~ normal( mu, sqrt(phi) ); // prior
  for ( t in 1:(nt-1) ) ITE[t] ~ normal(pte[t], sqrt( psi[t]) ); // prior
  sigma ~ inv_gamma(0.01, 10); // prior
  rho ~ uniform(-1, 1); // prior
  for (s in 1:ns) { // loop through the subjects
    for (i in first[s]:last[s]) { // loop through the rows
      irow = i-first[s]+1; // row index
      outsubj[irow] = outcome[index[i]]; // save individual outcome
      average[irow] = ALPHA[subj[index[i]]]; // add random intercept
      // add treatment effects
      for (t in 2:nt) average[irow] += ITE[t-1][subj[index[i]]]*(treat[index[i]]==t);
      COV[irow,irow] = sigma; // diagonal element
      j = i+1;
      while (j <= last[s]) { // loop through the columns
        icol = j-first[s]+1; // column index
        // upper diagonal element
        COV[irow,icol] = sigma*
          pow( rho, abs(period[index[j]]-period[index[i]]) );
        COV[icol,irow] = COV[irow,icol]; // lower diagonal element
        j += 1;
      }
    }
    // likelihood
    outsubj[1:irow] ~ multi_normal(average[1:irow], COV[1:irow,1:irow]);
  }
}
