/*
Written by Artur Araujo
November 2019

artur.stat@gmail.com

while working
on the IDEAL project

http://www.ideal.rwth-aachen.de/
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
		nmc=30000 /* number of mcmc iterations */
		nthreads=-1 /* number of parallel threads */
		ntu=1000 /* number of tuning iterations */
		seed=&seed /* random seed for simulation */
		thin=1; /* thinning rate */
	parms pte var_&Subject var_residual;
	
	beginnodata;
		prior pte ~ normal(mean=0, var=1e6);
		prior var_: ~ igamma(shape=0.01, scale=10);
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
