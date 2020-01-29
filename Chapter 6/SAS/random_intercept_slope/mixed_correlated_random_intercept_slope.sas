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

/* SAS program 6.12 */

title "Correlated random intercept and slope";

* define dataset names here ;
%let datain=WORK.gaba_placebo; * input dataset ;
%let datatemp=WORK.gaba_placebo_temp; * dummy dataset ;

* define variable names here ;
%let Subject=id; * subject variable ;
%let Treatment=Treatment; * treatment variable ;
%let Outcome=Pain; * outcome variable ;
%let ref=Placebo; * reference treatment ;

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

%macro sqlTreatmentLoop;
	%local t;
	%do t=2 %to &TreatmentNumber %by 1;
		case when &Treatment eq "&&Treatment&t"
		then 1 else 0 end as Treatment&&Treatment&t,
	%end;
%mend sqlTreatmentLoop;

* Create dataset with dummy variables ;
proc sql;
	create table &datatemp as
	select	&Subject,
			%sqlTreatmentLoop
			&Outcome
	from &datain;
quit;

%macro varTreatment;
	%local t;
	%do t=2 %to &TreatmentNumber %by 1;
		Treatment&&Treatment&t
	%end;
%mend varTreatment;

* Write global macro variable array ;
%estimate_random_slope_global(
	array=gaba_placebo_slope,
	subject=id,
	treatment=%varTreatment,
	data=&datatemp
)

* Fit linear mixed-effects model ;
proc mixed
		data=&datatemp
		alpha=0.05 /* level for CI */
		cl /* CI for covariance parameters */
		method=REML; * REML method for estimation ;
	class &Subject;
	model &Outcome = %varTreatment /
		alpha=0.05 /* level for CI */
		cl /* t-type CI for fixed effects */
		covb /* covariance matrix of fixed effects estimates */
		ddfm=KR /* inference based on Kenward and Roger's method */
		htype=2 /* type 2 hypothesis test */
		solution; * solution for fixed effects ;
	random intercept %varTreatment /
		subject=&Subject type=un;
	%estimate_random_slope(array=gaba_placebo_slope, options=alpha=0.05 cl)
run;

* Delete global macro variable array ;
%estimate_random_slope_free(array=gaba_placebo_slope)

* Delete datasets from system ;
proc sql;
	drop table &datatemp;
quit;

%symdel datain datatemp;
%symdel Subject Treatment Outcome ref;

%macro deleteVariables;
	%local i;
	%do i=1 %to &TreatmentNumber %by 1;
		%symdel Treatment&i;
	%end;
	%symdel TreatmentNumber;
%mend deleteVariables;

* Delete macro variables from system ;
%deleteVariables

* Delete macros from WORK.Sasmacr ;
proc catalog cat=WORK.Sasmacr;
	delete findRef sqlTreatmentLoop varTreatment
		deleteVariables / et=macro;
quit;

title; * clear all titles ;
