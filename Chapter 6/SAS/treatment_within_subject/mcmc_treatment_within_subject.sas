/*
Written by Artur Araujo
October-November 2019

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

/* SAS program G.1 */

title "Intercept varying among subject and treatment within subject";
title2 "Bayesian inference";

* define dataset names here ;
%let datain=WORK.gaba_placebo; * input dataset ;
%let datatemp=WORK.gaba_placebo_temp; * dummy dataset ;
%let dataout=WORK.outData; * output dataset ;
%let dataite=WORK.iteData; * individual treatment effect dataset ;

* define variable names here ;
%let Subject=id; * subject variable ;
%let Treatment=Treatment; * treatment variable ;
%let Cycle=Cycle; * cycle variable ;
%let Outcome=Pain; * outcome variable ;
%let ref=Placebo; * reference treatment ;
%let seed=3141593; * random seed for simulation ;

* Determine and count unique subjects and unique treatments;
proc sql noprint;
	select	distinct &Subject
			into :Subject1 - :Subject999999999
	from &datain;
	%let SubjectNumber = &SqlObs; * number of subjects ;
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

%macro sqlTreatmentLoop;
	%local t;
	case
	%do t = 1 %to &TreatmentNumber %by 1;
		when &Treatment eq "&&Treatment&t" then &t
	%end;
	end as &Treatment,
%mend sqlTreatmentLoop;

* Create dataset with dummy variables ;
proc sql;
	create table &datatemp as
	select	&Subject,
			%sqlTreatmentLoop
			input(catx('_', &Subject, &Treatment), $14.) as &Subject._&Treatment,
			input(catx('_', &Subject, &Cycle), $5.) as &Subject._&Cycle,
			&Outcome
	from &datain;
quit;

%macro mcmcParmsLoop;
	%local t;
	parms intercept
	%do t=2 %to &TreatmentNumber %by 1;
		pte_&&Treatment&t
	%end;
	;
	parms var_&Subject var_&Subject._&Cycle
		var_&Subject._&Treatment var_residual;
%mend mcmcParmsLoop;

%macro mcmcTreatmentLoop;
	%local t;
	%do t=2 %to &TreatmentNumber %by 1;
		+ pte_&&Treatment&t*(&Treatment eq &t)
	%end;
%mend mcmcTreatmentLoop;

* Fit linear mixed-effects model ;
proc mcmc
		data=&datatemp
		outpost=&dataout
		missing=COMPLETECASE /* discard missing observations */
		nbi=2000 /* number of burn-in iterations */
		nmc=100000 /* number of mcmc iterations */
		ntu=2000 /* number of turning iterations */
		seed=&seed /* random seed for simulation */
		thin=1 /* thinning rate */
		diagnostics=none /* suppress diagnostics */
		plots=none /* suppress plots */
		statistics=none; /* suppress posterior statistics */
	%mcmcParmsLoop
	
	beginnodata;
		prior intercept pte_: ~ normal(mean=0, var=1e7);
		prior var_: ~ igamma(shape=0.01, scale=0.01);
	endnodata;
	
	random bi ~ normal(mean=0, var=var_&Subject) subject=&Subject;
	random ci ~ normal(mean=0, var=var_&Subject._&Cycle) subject=&Subject._&Cycle;
	random di ~ normal(mean=0, var=var_&Subject._&Treatment) subject=&Subject._&Treatment;
	
	Mu = intercept %mcmcTreatmentLoop + bi + ci + di;
	model &Outcome ~ normal(mean=Mu, var=var_residual);
run;

%macro sqlFixedLoop;
	%local t;
	%do t=2 %to &TreatmentNumber %by 1;
		pte_&&Treatment&t,
	%end;
%mend sqlFixedLoop;

%macro sqlSubjectLoop;
	%local s t;
	%do s=1 %to &SubjectNumber %by 1;
		%do t=2 %to &TreatmentNumber %by 1;
			pte_&&Treatment&t +
			di_&&Subject&s.._&&Treatment&t - di_&&Subject&s.._&Treatment1
			as ite_&&Subject&s.._&&Treatment&t,
		%end;
	%end;
%mend sqlSubjectLoop;

* Create dataset with individual treatment effects ;
proc sql;
	create table &dataite as
	select 	Iteration,
			intercept, %sqlFixedLoop
			var_&Subject, var_&Subject._&Cycle,
			var_&Subject._&Treatment, var_residual,
			%sqlSubjectLoop
			LogPrior, LogReff,
			LogLike, LogPost
	from &dataout;
quit;

* Print posterior summaries and intervals ;
%sumint(data=&dataite, var=intercept pte_: var_: ite_:, alpha=0.05)

* Print effective sample size ;
%ess(data=&dataite, var=intercept pte_: var_: ite_:)

* Draw diagnostic plots ;
%tadplot(data=&dataite, var=intercept pte_: var_: ite_:);

* Delete datasets from system ;
proc sql;
	drop table &datatemp, &dataout, &dataite;
quit;

%symdel datain datatemp dataout dataite;
%symdel Subject Treatment Cycle Outcome ref seed;

%macro deleteVariables;
	%local i;
	%do i=1 %to &SubjectNumber %by 1;
		%symdel Subject&i;
	%end;
	%do i=1 %to &TreatmentNumber %by 1;
		%symdel Treatment&i;
	%end;
	%symdel SubjectNumber TreatmentNumber;
%mend deleteVariables;

* Delete macro variables from system ;
%deleteVariables

* Delete macros from WORK.Sasmacr ;
proc catalog cat=WORK.Sasmacr;
	delete findRef sqlTreatmentLoop mcmcParmsLoop
		mcmcTreatmentLoop sqlFixedLoop sqlSubjectLoop
		deleteVariables / et=macro;
quit;

title; * clear all titles ;
