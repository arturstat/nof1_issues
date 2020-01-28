/*
Written by Artur Araujo

artur.stat@gmail.com
aamarinhodearaujo1@sheffield.ac.uk

October-November 2019
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

* Define local macros folder ;
filename MACROS '..\macros';

* Include macros ;
%include MACROS('*.sas');

/* SAS program 6.10 */

title "Intercept varying among subject and treatment within subject";

* Write global macro variable array ;
%estimate_full_global(
	array=gaba_placebo_array,
	subject=id,
	treatment=Treatment,
	data=WORK.gaba_placebo,
	ref="Placebo"
)

* Fit linear mixed-effects model ;
proc mixed
		data=WORK.gaba_placebo
		alpha=0.05 /* level for CI */
		cl /* CI for covariance parameters */
		method=REML; * REML method for estimation ;
	class Treatment (ref="Placebo") id Cycle;
	model Pain = Treatment /
		alpha=0.05 /* level for CI */
		cl /* t-type CI for fixed effects */
		covb /* covariance matrix of fixed effects estimates */
		ddfm=KR /* inference based on Kenward and Roger's method */
		htype=2 /* type 2 hypothesis test */
		solution; * solution for fixed effects ;
	random id Treatment*id Cycle*id;
	%estimate_full(array=gaba_placebo_array, options=alpha=0.05 cl)
run;

* Delete global macro variable array ;
%estimate_full_free(array=gaba_placebo_array)

/* SAS program 6.11 */

title "Intercept varying among subject and treatment within subject";
title2 "AR1 stochastic process";
title3 "random statement with subject option";

* Write global macro variable array ;
%estimate_full_random_global(
	array=gaba_placebo_random,
	subject=id,
	treatment=Treatment,
	data=WORK.gaba_placebo,
	ref="Placebo"
)

* Fit linear mixed-effects model ;
proc mixed
		data=WORK.gaba_placebo
		alpha=0.05 /* level for CI */
		cl /* CI for covariance parameters */
		method=REML; * REML method for estimation ;
	class Treatment (ref="Placebo") id Period;
	model Pain = Treatment /
		alpha=0.05 /* level for CI */
		cl /* t-type CI for fixed effects */
		covb /* covariance matrix of fixed effects estimates */
		ddfm=KR /* inference based on Kenward and Roger's method */
		htype=2 /* type 2 hypothesis test */
		solution; * solution for fixed effects ;
	random intercept Treatment /
		subject=id;
	repeated Period /
		subject=id
		type=ar(1)
		r
		rcorr;
	%estimate_full_random(array=gaba_placebo_random, options=alpha=0.05 cl)
run;

* Delete global macro variable array ;
%estimate_full_random_free(array=gaba_placebo_random)

title; * clear all titles ;
