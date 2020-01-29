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
%include '.\data_normal.sas';

/* SAS program 6.15 */

* define dataset names here ;
%let datain=WORK.gaba_placebo_diff; * input dataset ;
%let dataout=WORK.outRI; * output dataset ;
%let datasum=WORK.sumRI; * summary statistics dataset ;
%let dataplot=WORK.plotRI; * plot dataset ;

* define variable names here ;
%let Subject=id; * subject variable ;
%let Outcome=deltaPain; * outcome variable ;
%let seed=3141593; * random seed for simulation ;

* Fit linear mixed-effects model ;
proc mcmc
		data=&datain
		outpost=&dataout
		missing=COMPLETECASE /* discard missing observations */
		nbi=1000 /* number of burn-in iterations */
		nmc=100000 /* number of mcmc iterations */
		ntu=1000 /* number of turning iterations */
		seed=&seed /* random seed for simulation */
		thin=1 /* thinning rate */
		diagnostics=none /* suppress diagnostics */
		plots=none /* suppress plots */
		statistics=none; /* suppress posterior statistics */
	parms pte var_&Subject var_residual;
	
	beginnodata;
		prior pte ~ normal(mean=0, var=1e7);
		prior var_&Subject ~ igamma(shape=0.01, scale=0.01);
		prior var_residual ~ igamma(shape=0.01, scale=0.01);
	endnodata;
	
	random ite ~ normal(mean=pte, var=var_&Subject)
		subject=&Subject monitor=(ite);
	
	model &Outcome ~ normal(mean=ite, var=var_residual);
run;

* Create dataset with posterior summaries and intervals ;
%sumint(data=&dataout, var=pte ite_:, print=no, out=&datasum, alpha=0.05)

* Create plot dataset ;
proc sql;
	create table &dataplot as
	select	*,
			"Mean" as estimate,
			"Lower" as lower,
			"Upper" as upper,
			"SD" as sd
	from (
		select	"population" as id,
				Mean as MeanP format=8.2,
				StdDev as StdDevP format=8.2,
				HPDLower as HPDLowerP format=8.2,
				HPDUpper as HPDUpperP format=8.2
		from &datasum
		where Parameter eq "pte"
		outer union corresponding
		select	substring(Parameter from 5) as id,
				Mean format=8.2,
				StdDev format=8.2,
				HPDLower format=8.2,
				HPDUpper format=8.2
		from &datasum
		where Parameter ne "pte"
	)
	order by HPDUpper;
quit;

* Set graph size in pixels ;
ods graphics on /
	border=off /* suppress graphics border */
	width=1080px
	height=1080px;

title; * clear all titles ;

* Draw forest plot of individual treatment effects ;
proc sgplot
		data=&dataplot
		description="Forest plot of individual treatment effects"
		noautolegend
		noborder;
	scatter y=id x=Mean /
		xerrorupper=HPDUpper
		xerrorlower=HPDLower
		errorbarattrs=(color=blue thickness=1px pattern=Solid)
		noerrorcaps
		markerattrs=(color=black size=7px symbol=CircleFilled);
	scatter y=id x=MeanP /
		xerrorupper=HPDUpperP
		xerrorlower=HPDLowerP
		errorbarattrs=(color=red thickness=1px pattern=Solid)
		noerrorcaps
		markerattrs=(color=black size=7px symbol=DiamondFilled);
	scatter y=id x=estimate / markerchar=Mean x2axis;
	scatter y=id x=lower / markerchar=HPDLower x2axis;
	scatter y=id x=upper / markerchar=HPDUpper x2axis;
	scatter y=id x=sd / markerchar=StdDev x2axis;
	scatter y=id x=estimate / markerchar=MeanP x2axis;
	scatter y=id x=lower / markerchar=HPDLowerP x2axis;
	scatter y=id x=upper / markerchar=HPDUpperP x2axis;
	scatter y=id x=sd / markerchar=StdDevP x2axis;
	refline 0 /
		axis=x
		lineattrs=(color=gray thickness=1px pattern=Dash);
	inset 'Favors Placebo' / position=bottomleft;
	inset 'Favors Gabapentin' / position=bottom;
	xaxis
		offsetmin=0 offsetmax=0.35
		values=(-4 to 2 by 1)
		display=(nolabel);
	x2axis
		offsetmin=0.7
		display=(nolabel noline noticks);
	yaxis
		offsetmin=0.05 offsetmax=0.025
		type=discrete
		display=(nolabel noline noticks);
run;

ods graphics off;

* Delete datasets from system ;
proc sql;
	drop table &dataout, &datasum, &dataplot;
quit;

%symdel datain dataout datasum dataplot;
%symdel Subject Outcome seed;

title; * clear all titles ;
