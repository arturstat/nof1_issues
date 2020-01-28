* Written by Artur Araujo ;
* November 2019 ;

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
%include '..\data.sas';

/* SAS program G.9 */

title "Random intercepts - jointmodel";
title2 "AR1 stochastic process";
title3 "Bayesian inference";

* define dataset names here ;
%let datain=WORK.gaba_placebo_diff; * input dataset ;
%let datamiss=WORK.gaba_diff_miss; * no missing dataset ;
%let datadummy=WORK.dummy; * dummy dataset ;
%let dataout=WORK.gaba_joint_ar1; * output dataset ;

%let Subject=id; * subject variable ;
%let Cycle=Cycle; * cycle variable ;
%let Period=deltaPeriod; * delta period variable ;
%let Outcome=deltaPain; * outcome variable ;
%let seed=3141593; * random seed for simulation ;

* Create dataset without missing observations ;
proc sql;
	create table &datamiss as
	select	&Subject,
			&Cycle,
			&Period,
			&Outcome
	from &datain
	where &Outcome is not null
	order by &Subject, &Cycle;
quit;

* Determine and count unique subjects, and number of observations ;
proc sql noprint;
	select	distinct &Subject
			into :Subject1 - :Subject999999999
	from &datamiss;
	%let SubjectNumber = &SqlObs; * number of subjects ;
	select count(&Outcome) into :size1 - :size999999999
	from &datamiss group by &Subject; * number of observations per subject ;
	select count(*) into :nobs /* total number of observations */
	from &datamiss;
quit;

* Create dummy dataset ;
data &datadummy;
run;

%macro mcmcArray;
	%local s;
	%do s = 1 %to &SubjectNumber %by 1;
		array Y_&&Subject&s..[&&size&s];
		array MU_&&Subject&s..[&&size&s];
		array COV_&&Subject&s..[&&size&s,&&size&s];
	%end;
	array P[&nobs] / nosymbols;
	array data[&nobs] / nosymbols;
	begincnst;
		rc = read_array("&datamiss", P, "&Period");
		rc = read_array("&datamiss", data, "&Outcome");
	endcnst;
%mend mcmcArray;

%macro mcmcParms;
	%local s;
	parms pte var_&Subject;
	parms var_residual rho /slice;
	parms
	%do s = 1 %to &SubjectNumber %by 1;
		ite_&&Subject&s
	%end;
	;
%mend mcmcParms;

%macro mcmcLogLike;
	%local s;
	index = 1;
	llike = 0;
	%do s = 1 %to &SubjectNumber %by 1;
		do i = 1 to &&size&s by 1;
			Y_&&Subject&s..[i] = data[index];
			MU_&&Subject&s..[i] = ite_&&Subject&s;
			COV_&&Subject&s..[i,i] = 2*var_residual*(1-rho);
			do j = i+1 to &&size&s by 1;
				COV_&&Subject&s..[i,j] = -P[index]*P[index+j-i]*var_residual*(
					rho**(2*abs(i-j)+1)
					- 2*rho**( 2*abs(i-j) )
					+ rho**(2*abs(i-j)-1)
				);
				COV_&&Subject&s..[j,i] = COV_&&Subject&s..[i,j];
			end;
			index = index + 1;
		end;
		llike = llike + lpdfmvn(Y_&&Subject&s, MU_&&Subject&s, COV_&&Subject&s);
	%end;
	model general(llike);
%mend mcmcLogLike;

* Fit linear mixed-effects model ;
proc mcmc
		data=&datadummy
		outpost=&dataout
		nbi=10000 /* number of burn-in iterations */
		nmc=10000 /* number of mcmc iterations */
		ntu=1000 /* number of turning iterations */
		seed=&seed /* random seed for simulation */
		thin=1 /* thinning rate */
		jointmodel; /* specify joint log-likelihood */
	%mcmcArray
	
	%mcmcParms
	
	beginnodata;
		hyperprior pte ~ normal(mean=0, var=1e7);
		hyperprior var_&Subject ~ igamma(shape=0.01, scale=0.01);
		prior ite_: ~ normal(mean=pte, var=var_&Subject);
		prior var_residual ~ igamma(shape=0.01, scale=0.01);
		prior rho ~ uniform(left=-1, right=1);
	endnodata;
	
	%mcmcLogLike
run;

* Delete datasets from system ;
proc sql;
	drop table &datamiss, &datadummy, &dataout;
quit;

%symdel datain datamiss datadummy dataout;
%symdel Subject Cycle Period Outcome seed nobs;

%macro deleteVariables;
	%local i;
	%do i = 1 %to &SubjectNumber %by 1;
		%symdel Subject&i size&i;
	%end;
	%symdel SubjectNumber;
%mend deleteVariables;

* Delete macro variables from system ;
%deleteVariables

* Delete macros from WORK.Sasmacr ;
proc catalog cat=WORK.Sasmacr;
	delete mcmcArray mcmcParms mcmcLogLike
		deleteVariables / et=macro;
quit;

title; * clear all titles ;
