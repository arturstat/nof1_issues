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

/* SAS program G.3 */

title "Correlated random intercept and slope";
title2 "Bayesian inference";

* define dataset names here ;
%let datain=WORK.gaba_placebo; * input dataset ;
%let datatemp=WORK.gaba_placebo_temp; * dummy dataset ;
%let dataout=WORK.outDataCRIS; * output dataset ;

* define variable names here ;
%let Subject=id; * subject variable ;
%let Treatment=Treatment; * treatment variable ;
%let Outcome=Pain; * outcome variable ;
%let ref=Placebo; * reference treatment ;
%let seed=3141593; * random seed for simulation ;

* Determine and count unique treatments;
proc sql noprint;
	select distinct &Treatment
			into :Treatment1 - :Treatment999999999
	from &datain;
	%let TreatmentNumber = &SqlObs; * number of treatments ;
quit;

%macro findRef;
	%if &Treatment1 ne &ref %then %do;
		%local t f;
		%let t=1;
		%do %until (&t eq &TreatmentNumber or &&Treatment&t eq &ref);
			%let t=%eval(&t+1);
		%end;
		%let f=&Treatment1;
		%let Treatment1=&ref;
		%let Treatment&t=&f;
	%end;
%mend findRef;

* Set first treatment to reference treatment ;
%findRef

%macro dataTemp;
	%local t;
	proc sql;
		create table &datatemp as
		select 	&Subject,
				case
				%do t = 1 %to &TreatmentNumber %by 1;
					when &Treatment eq "&&Treatment&t" then &t
				%end;
				end as &Treatment,
				&Outcome
		from &datain;
	quit;
%mend dataTemp;

* Convert treatment variable to numeric ;
%dataTemp

%macro mcmcArray;
	%local t;
	array R[&TreatmentNumber] bi
	%do t = 2 %to &TreatmentNumber %by 1;
		ite_&&Treatment&t
	%end;
	;
	array B[&TreatmentNumber] intercept
	%do t = 2 %to &TreatmentNumber %by 1;
		pte_&&Treatment&t
	%end;
	;
	array G[&TreatmentNumber,&TreatmentNumber];
	array M[&TreatmentNumber];
	array S[&TreatmentNumber,&TreatmentNumber];
	array U[&TreatmentNumber,&TreatmentNumber];
%mend mcmcArray;

%macro mcmcMonitor;
	%local t;
	%do t = 2 %to &TreatmentNumber %by 1;
		ite_&&Treatment&t
	%end;
%mend mcmcMonitor;

%macro mcmcMU;
	%local t;
	mu = bi
	%do t = 2 %to &TreatmentNumber %by 1;
		+ ite_&&Treatment&t*(&Treatment eq &t)
	%end;
	;
%mend mcmcMU;

* Fit linear mixed-effects model ;
proc mcmc
		data=&datatemp
		outpost=&dataout
		missing=COMPLETECASE /* discard missing observations */
		nbi=1000 /* number of burn-in iterations */
		nthreads=-1 /* number of parallel threads */
		nmc=100000 /* number of mcmc iterations */
		ntu=1000 /* number of tuning iterations */
		seed=&seed /* random seed for simulation */
		thin=1; /* thinning rate */
	%mcmcArray
	
	begincnst;
		call zeromatrix(M);
		call identity(S);
		call mult(S, 1e6, S);
		call identity(U);
		call mult(U, 10, U);
	endcnst;
	
	parms B;
	parms G var_residual;
	
	beginnodata;
		prior B ~ mvn(M, S);
		prior G ~ iwish(&TreatmentNumber, U);
		prior var_residual ~ igamma(0.01, scale=10);
	endnodata;
	
	random R ~ mvn(B, G) subject=&Subject monitor=(%mcmcMonitor);
	
	%mcmcMU
	model &Outcome ~ normal(mu, var=var_residual);
run;

* Delete datasets from system ;
proc sql;
	drop table &datatemp, &dataout;
quit;

%symdel datain datatemp dataout;
%symdel Subject Treatment Outcome ref seed;

%macro deleteVariables;
	%do i = 1 %to &TreatmentNumber %by 1;
		%symdel Treatment&i;
	%end;
	%symdel TreatmentNumber;
%mend deleteVariables;

* Delete macro variables from system ;
%deleteVariables

* Delete macros from WORK.Sasmacr ;
proc catalog cat=WORK.Sasmacr;
	delete findRef dataTemp mcmcArray
		mcmcMonitor mcmcMU deleteVariables / et=macro;
quit;

title; * clear all titles ;
