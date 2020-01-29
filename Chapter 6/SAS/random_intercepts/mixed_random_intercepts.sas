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

* Define local macros folder ;
filename MACROS '..\macros';

* Include macros ;
%include MACROS('*.sas');

/* SAS program 6.13 */

title "Random intercepts";

* Write global macro variable array ;
%estimate_diff_global(
	array=diff_array,
	subject=id,
	data=WORK.gaba_placebo_diff
)

* Fit linear mixed-effects model ;
proc mixed
		data=WORK.gaba_placebo_diff
		alpha=0.05 /* level for CI */
		cl /* CI for covariance parameters */
		method=REML; * REML method for estimation ;
	class id;
	model deltaPain = /
		alpha=0.05 /* level for CI */
		cl /* t-type CI for fixed effects */
		covb /* covariance matrix of fixed effects estimates */
		ddfm=KR /* inference based on Kenward and Roger's method */
		solution; * solution for fixed effects ;
	random id;
	* insert estimate statements ;
	%estimate_diff(array=diff_array, options=alpha=0.05 cl)
run;

* Delete global macro variable array ;
%estimate_diff_free(array=diff_array)

/* SAS program 6.14 */

title "Random intercepts";
title2 "random statement with subject option";

* Write global macro variable array ;
%estimate_diff_random_global(
	array=diff_random_array,
	subject=id,
	data=WORK.gaba_placebo_diff
)

* Fit linear mixed-effects model ;
proc mixed
		data=WORK.gaba_placebo_diff
		alpha=0.05 /* level for CI */
		cl /* CI for covariance parameters */
		method=REML; * REML method for estimation ;
	class id;
	model deltaPain = /
		alpha=0.05 /* level for CI */
		cl /* t-type CI for fixed effects */
		covb /* covariance matrix of fixed effects estimates */
		ddfm=KR /* inference based on Kenward and Roger's method */
		solution; * solution for fixed effects ;
	random intercept /
		subject=id;
	* insert estimate statements ;
	%estimate_diff_random(array=diff_random_array, options=alpha=0.05 cl)
run;

* Delete global macro variable array ;
%estimate_diff_random_free(array=diff_random_array)

title; * clear all titles ;
