/*
Written by Artur Araujo

artur.stat@gmail.com
aamarinhodearaujo1@sheffield.ac.uk

October 2019
*/

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
%include '.\data_normal.sas';

/* SAS program 6.1 */

title "Two-sided paired t-test 5% significance level";
proc ttest
		data=WORK.gaba_placebo_diff
		alpha=0.05
		dist=normal
		h0=0
		sides=2
		test=diff
		ci=none
		plots=none;
	paired Pain_Gabapentin*Pain_Placebo;
run;

/* SAS program 6.2 */

title "Two-sided one-sample t-test 5% significance level";
proc ttest
		data=WORK.gaba_placebo_diff
		alpha=0.05
		dist=normal
		h0=0
		sides=2
		ci=none
		plots=none;
	var deltaPain;
run;

title; * clear all titles ;
