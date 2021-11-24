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

/* Stan program I.8 */

data {
  int<lower=3> nd; // number of data elements
  int<lower=1> ns; // number of subjects
  int<lower=1> nc; // maximum number of cycles
  int<lower=1, upper=ns> subj[nd]; // subject indicator
  int<lower=1, upper=nc> cycle[nd]; // cycle indicator
  int<lower=-1, upper=1> delta[nd]; // delta array
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
  real pte; // intercept
  real<lower=0> psi; // between subject variance
  vector[ns] ITE; // random effects
  real<lower=0> sigma; // residual variance
  real<lower=-1, upper=1> rho; // correlation coefficient
}

model {
  vector[nmax] outsubj; // individual outcome
  vector[nmax] average; // individual mean
  matrix[nmax,nmax] COV; // individual covariance matrix
  int irow; int icol; // row and column indexes
  int j; int k;
  pte ~ normal(0, 1e6); // hyperprior
  psi ~ inv_gamma(0.01, 10); // hyperprior
  ITE ~ normal( pte, sqrt(psi) ); // subject random effects
  sigma ~ inv_gamma(0.01, 10); // prior
  rho ~ uniform(-1, 1); // prior
  for (s in 1:ns) { // loop through the subjects
    for (i in first[s]:last[s]) { // loop through the rows
      irow = i-first[s]+1; // row index
      outsubj[irow] = outcome[index[i]]; // save individual outcome
      average[irow] = ITE[subj[index[i]]]; // random intercept
      COV[irow,irow] = 2*sigma*(1-rho); // diagonal element
      j = i+1;
      while (j <= last[s]) { // loop through the columns
        icol = j-first[s]+1; // column index
        k = 2*abs(cycle[index[j]]-cycle[index[i]]);
        // upper diagonal element
        COV[irow,icol] = -delta[index[j]]*delta[index[i]]*sigma*
          pow(rho, k-1)*pow(1-rho, 2);
        COV[icol,irow] = COV[irow,icol]; // lower diagonal element
        j += 1;
      }
    }
    // likelihood
    outsubj[1:irow] ~ multi_normal(average[1:irow], COV[1:irow,1:irow]);
  }
}
