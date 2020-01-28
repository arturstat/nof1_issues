/*
Written by Artur Araujo

artur.stat@gmail.com
aamarinhodearaujo1@sheffield.ac.uk

November 2019
*/

* Options for macro debugging ;
*options symbolgen mprint mlogic;
*option nosymbolgen nomprint nomlogic;

* Log current folder ;
%xlog(cd);

* Determine path to script ;
%let ScriptDir=%sysfunc(
	tranwrd(
		%sysget(SAS_EXECFILEPATH),
		%sysget(SAS_EXECFILENAME),
	)
);

* Set current folder ;
x cd "&ScriptDir";

* Log current folder ;
%xlog(cd);

* Include data ;
%include '..\data_normal.sas';

/* SAS program G.7 */

title "Random intercepts";
title2 "Bayesian inference";

* define dataset names here ;
%let datain=WORK.gaba_placebo_diff; * input dataset ;
%let dataout=WORK.outRI; * output dataset ;

* define variable names here ;
%let Subject=id; * subject variable ;
%let Outcome=deltaPain; * outcome variable ;
%let seed=3141593; * random seed for simulation ;

* Fit linear mixed-effects model ;
proc mcmc
		data=&datain
		outpost=&dataout
		missing=COMPLETECASE /* discard missing observations */
		nbi=1000 /* number of burn-in iterations */
		nmc=100000 /* number of mcmc iterations */
		ntu=1000 /* number of turning iterations */
		seed=&seed /* random seed for simulation */
		thin=1; /* thinning rate */
	parms pte var_&Subject var_residual;
	
	beginnodata;
		prior pte ~ normal(mean=0, var=1e7);
		prior var_&Subject ~ igamma(shape=0.01, scale=0.01);
		prior var_residual ~ igamma(shape=0.01, scale=0.01);
	endnodata;
	
	random ite ~ normal(mean=pte, var=var_&Subject)
		subject=&Subject monitor=(ite);
	
	model &Outcome ~ normal(mean=ite, var=var_residual);
run;

* Delete datasets from system ;
proc sql;
	drop table &dataout;
quit;

%symdel datain dataout;
%symdel Subject Outcome seed;

title; * clear all titles ;
