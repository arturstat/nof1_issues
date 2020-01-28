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

/* SAS program 6.4 */

title "ANOVA Treatment by Subject interaction - proc glm";
proc glm data=WORK.gaba_placebo_miss plots=none;
	class Treatment id;
	model Pain = Treatment id Treatment*id / ss1 ss2 ss3 ss4;
	lsmeans Treatment /
		alpha=0.05 /* level for CI */
		bylevel /* weight by each level of the LS-mean effect*/
		cl /* confidence limits */
		stderr; /* standard error */
run;
quit;

title; * clear all titles ;
