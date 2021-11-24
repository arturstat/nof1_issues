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
%include '..\data.sas';

/* SAS program G.8 */

title "Random intercepts";
title2 "AR1 stochastic process";
title3 "Bayesian inference";

%let datain=WORK.gaba_placebo_diff; * input dataset ;
%let datatemp=WORK.gaba_placebo_temp; * temporary dataset ;
%let dataout=WORK.gaba_placebo_riar1; * output dataset ;

%let Subject=id; * subject variable ;
%let Cycle=Cycle; * cycle variable ;
%let Period=deltaPeriod; * delta period variable ;
%let Outcome=deltaPain; * outcome variable ;
%let seed=3141593; * random seed for simulation ;

* Order dataset by &Subject and &Cycle ;
proc sort data=&datain;
	by &Subject &Cycle;
run;

* Read number of cycles into macro variable ;
proc sql noprint;
	select max(&Cycle) into :CycleNumber
	from &datain;
quit;

* Create dataset with one observation per subject ;
data &datatemp;
	set &datain;
	by &Subject;
	array Y[&CycleNumber];
	array P[&CycleNumber];
	retain Y1-Y%eval(&CycleNumber) P1-P%eval(&CycleNumber);
	if first.&Subject then do;
		call missing( of Y1-Y%eval(&CycleNumber) );
		call missing( of P1-P%eval(&CycleNumber) );
	end;
	Y[Cycle] = &Outcome;
	P[Cycle] = &Period;
	if last.&Subject then output;
	keep &Subject Y1-Y%eval(&CycleNumber) P1-P%eval(&CycleNumber);
run;

* Fit linear mixed-effects model ;
proc mcmc
		data=&datatemp
		outpost=&dataout
		nbi=1000 /* number of burn-in iterations */
		nmc=100000 /* number of mcmc iterations */
		nthreads=-1 /* number of parallel threads */
		ntu=2000 /* number of tuning iterations */
		seed=&seed /* random seed for simulation */
		thin=1; /* thinning rate */
	array Y[&CycleNumber] Y1-Y%eval(&CycleNumber);
	array Mu[&CycleNumber];
	array Cov[&CycleNumber,&CycleNumber];
	array P[&CycleNumber] P1-P%eval(&CycleNumber);
	
	parms pte var_&Subject; * separate Conjugate sampling ;
	parms var_residual / slice; * separate Slice sampling ;
	parms rho / slice; * separate Slice sampling ;
	
	beginnodata;
		prior pte ~ normal(0, var=1e6);
		prior var_: ~ igamma(shape=0.01, scale=10);
		prior rho ~ uniform(left=-1, right=1);
	endnodata;
	
	random ite ~ normal(pte, var=var_&Subject)
		subject=&Subject monitor=(ite);
	
	do i = 1 to &CycleNumber;
		Mu[i] = ite;
		Cov[i,i] = 2*var_residual*(1-rho);
		do j = i+1 to &CycleNumber;
			Cov[i,j] = -P[i]*P[j]*var_residual*
				rho**(2*abs(i-j)-1)*(1-rho)**2;
			Cov[j,i] = Cov[i,j];
		end;
	end;
	
	model Y ~ mvn(Mu, Cov);
run;

* Delete datasets from system ;
proc sql;
	drop table &datatemp, &dataout;
quit;

%symdel datain datatemp dataout;
%symdel Subject Cycle Period Outcome seed CycleNumber;

title; * clear all titles ;
