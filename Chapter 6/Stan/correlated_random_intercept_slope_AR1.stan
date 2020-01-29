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

/* Stan program I.4 */

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
  vector[nt] THETA = rep_vector(0, nt); // MU prior mean
  matrix[nt,nt] ETA = diag_matrix( rep_vector(1e7, nt) ); // MU prior covariance
  matrix[nt,nt] DELTA = diag_matrix( rep_vector(0.01, nt) ); // PSI prior matrix
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
  vector[nt] MU; // fixed effects
  cov_matrix[nt] PSI; // random effects cov_matrix
  vector[nt] ALPHA[ns]; // random effects
  real<lower=0> sigma; // residual sd
  real<lower=-1, upper=1> rho; // correlation coefficient
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
  vector[nmax] outsubj; // individual outcome
  vector[nmax] average; // individual mean
  matrix[nmax,nmax] COV; // individual covariance matrix
  int irow; // row index
  int icol; // column index
  int j; // while loop counter
  MU ~ multi_normal(THETA, ETA); // hyperprior
  PSI ~ inv_wishart(nt, DELTA); // hyperprior
  ALPHA ~ multi_normal(MU, PSI); // prior
  sigma ~ inv_gamma(0.01, 0.01); // prior
  rho ~ uniform(-1, 1); // prior
  for (s in 1:ns) { // loop through the subjects
    for (i in first[s]:last[s]) { // loop through the rows
      irow = i-first[s]+1; // row index
      outsubj[irow] = outcome[index[i]]; // save individual outcome
      average[irow] = ALPHA[subj[index[i]]][1]; // add random intercept
      // add treatment effects
      for (t in 2:nt) average[irow] += ALPHA[subj[index[i]]][t]*(treat[index[i]]==t);
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
