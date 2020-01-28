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

/* SAS program 6.3 */

proc sql;
	create table WORK.gaba_placebo_sum as
	select id,
	mean(deltaPain) as meanPain
	from WORK.gaba_placebo_diff
	group by id;
quit;

title "Summary measures approach";
proc ttest
		data=WORK.gaba_placebo_sum
		alpha=0.05
		dist=normal
		h0=0
		sides=2
		ci=none
		plots=none;
	var meanPain;
run;

proc sql;
	drop table WORK.gaba_placebo_sum;
quit;

title; * clear all titles ;
