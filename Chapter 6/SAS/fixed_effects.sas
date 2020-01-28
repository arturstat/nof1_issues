/*
Written by Artur Araujo

artur.stat@gmail.com
aamarinhodearaujo1@sheffield.ac.uk

December 2019
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
%include '.\data_normal.sas';

/*********************************************
*** fixed treatment by subject interaction ***
*********************************************/

/* SAS program 6.5 */

title "Fixed treatment by subject interaction";
title2 "Weighted estimate";

proc glm
		data=WORK.gaba_placebo_miss
		plots=none; * suppress plots ;
	class Treatment (ref="Placebo") id;
	model Pain = Treatment id Treatment*id;
	lsmeans Treatment /
		alpha=0.05 /* level for CI */
		bylevel /* weight by each level of the LS-mean effect*/
		cl /* confidence limits */
		pdiff /* p-values for differences */
		stderr /* standard error */
		tdiff; /* t values for differences */
run;
quit;

/*********************************************
*** fixed treatment by subject interaction ***
** random cycle by subject interaction *******
*********************************************/

/* SAS program 6.6 */

title "Fixed treatment by subject interaction";
title2 "Random cycle by subject interaction";
title3 "Weighted estimate";

proc mixed
		data=WORK.gaba_placebo_miss
		method=REML; * REML method for estimation ;
	class Treatment (ref="Placebo") id Cycle;
	model Pain = Treatment id Treatment*id /
		ddfm=KR /* inference based on Kenward and Roger's method */
		htype=2; /* type 2 hypothesis test */
	random Cycle / subject=id;
	lsmeans Treatment /
		alpha=0.05 /* level for CI */
		bylevel /* weight by each level of the LS-mean effect*/
		cl /* confidence limits */
		diff; /* differences of LS-means */
run;

/*********************************************
*** fixed treatment by subject interaction ***
** AR(1) stochastic process ******************
*********************************************/

/* SAS program 6.7 */

title "Fixed treatment by subject interaction";
title2 "AR(1) stochastic process";
title3 "Weighted estimate";

proc mixed
		data=WORK.gaba_placebo_miss
		method=REML; * REML method for estimation ;
	class Treatment (ref="Placebo") id Period;
	model Pain = Treatment id Treatment*id /
		ddfm=KR /* inference based on Kenward and Roger's method */
		htype=2; /* type 2 hypothesis test */
	repeated Period /
		subject=id type=ar(1) r=3 rcorr=3;
	lsmeans Treatment /
		alpha=0.05 /* level for CI */
		bylevel /* weight by each level of the LS-mean effect*/
		cl /* confidence limits */
		diff; /* differences of LS-means */
run;

/*********************************************
*** fixed treatment by subject interaction ***
** distinct residual variance per cycle ******
*********************************************/

/* SAS program 6.8 */

title "Fixed treatment by subject interaction";
title2 "Distinct residual variance per cycle";
title3 "Unweighted estimate";

proc mixed
		data=WORK.gaba_placebo_miss
		method=REML; * REML method for estimation ;
	class Treatment (ref="Placebo") id Cycle;
	model Pain = Treatment id Treatment*id /
		ddfm=KR /* inference based on Kenward and Roger's method */
		htype=2; /* type 2 hypothesis test */
	repeated /
		group=Cycle;
	estimate 'Gabapentin-Placebo' Treatment 1 -1 / alpha=0.05 cl;
run;

/***************************
*** fixed subject effect ***
***************************/

/* SAS program 6.9 */

%macro diff_estimate_global(
	array, /* prefix of global macro variables */
	subject, /* subject variable */
	outcome, /* outcome variable */
	data /* dataset name */
);
	proc sql noprint;
		select	count(&outcome) into :n1 - :n999999999
		from &data
		group by &subject;
		%let s = &SqlObs; * number of subjects ;
	quit;
	%global &array._subject &array._r &array._sum;
	%let &array._subject = &subject;
	%let &array._r = ;
	%let &array._sum = 0;
	%do i = 1 %to &s %by 1;
		%let &array._r = &&&array._r &&n&i;
		%let &array._sum = %eval(&&&array._sum + &&n&i);
	%end;
%mend diff_estimate_global;

%macro diff_estimate(
	array, /* prefix of global macro variables */
	label, /* label for estimate statement */
	options /* options for estimate statement */
);
	estimate &label
		intercept			&&&array._sum
		&&&array._subject	&&&array._r /
		divisor=&&&array._sum &options;
%mend diff_estimate;

%macro diff_estimate_free(
	array /* prefix of global macro variables */
);
	%symdel &array._subject &array._r &array._sum;
%mend diff_estimate_free;

title "Fixed subject effect";
title2 "Weighted estimate";

%diff_estimate_global(
	array=gaba_placebo_diff,
	subject=id,
	outcome=deltaPain,
	data=WORK.gaba_placebo_diff
)

proc glm
		data=WORK.gaba_placebo_diff
		alpha=0.05 /* level for CI */
		plots=none; * suppress plots ;
	class id;
	model deltaPain = id /
		alpha=0.05 /* level for CI */
		clparm; /* CI for parameter estimates */
	%diff_estimate(
		array=gaba_placebo_diff,
		label='Gabapentin-Placebo'
	)
run;
quit;

%diff_estimate_free(array=gaba_placebo_diff)

title; * clear all titles ;
