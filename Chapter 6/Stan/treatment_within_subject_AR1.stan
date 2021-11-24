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

/* Stan program I.2 */

data {
  int<lower=3> nd; // number of data elements
  int<lower=1> ns; // number of subjects
  int<lower=2> nt; // number of treatments
  int<lower=2> nm; // number of subject*treatment levels
  int<lower=1> np; // maximum number of periods
  int<lower=1, upper=ns> subj[nd]; // subject indicator
  int<lower=1, upper=nt> treat[nd]; // treatment indicator
  int<lower=1, upper=nm> subj_treat[nd]; // subject*treatment indicator
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
  real<lower=0> phi; // between subject variance
  real<lower=0> psi; // subject*treatment variance
  real<lower=0> sigma; // residual variance
  real<lower=-1, upper=1> rho; // correlation coefficient
  vector[ns] ALPHA; // subject random effects
  vector[nm] THETA; // subject*treatment random effects
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
  vector[nmax] outsubj; // individual outcome
  vector[nmax] average; // individual mean
  matrix[nmax,nmax] COV; // individual covariance matrix
  int irow; // row index
  int icol; // column index
  int j; // while loop counter
  mu ~ normal(0, 1e6); // prior
  pte ~ normal(0, 1e6); // prior
  phi ~ inv_gamma(0.01, 10); // hyperprior
  psi ~ inv_gamma(0.01, 10); // hyperprior
  sigma ~ inv_gamma(0.01, 10); // prior
  rho ~ uniform(-1, 1); // prior
  ALPHA ~ normal( 0, sqrt(phi) ); // subject random effects
  THETA ~ normal( 0, sqrt(psi) ); // subject*treatment random effects
  for (s in 1:ns) { // loop through the subjects
    for (i in first[s]:last[s]) { // loop through the rows
      irow = i-first[s]+1; // row index
      outsubj[irow] = outcome[index[i]]; // save individual outcome
      // add intercept plus subject random effect
      average[irow] = mu+ALPHA[subj[index[i]]];
      // add treatment fixed effects
      for (t in 2:nt) average[irow] += pte[t-1]*(treat[index[i]]==t);
      // add subject*treatment interaction
      average[irow] += THETA[subj_treat[index[i]]];
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
