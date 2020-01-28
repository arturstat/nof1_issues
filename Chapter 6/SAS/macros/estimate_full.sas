/*
Written by Artur Araujo
May 2016

Modified by Artur Araujo
April 2018
November 2019

artur.stat@gmail.com
aamarinhodearaujo1@sheffield.ac.uk

while working for the
Luxembourg Institute of Health
on the IDEAL project

http://www.ideal.rwth-aachen.de/
*/

* Options for macro debugging ;
*options symbolgen mprint mlogic;

/*
The %estimate_full_global macro creates a global array of
macro variables. This global array of macro variables can
later be supplied to the %estimate_full macro developed
to insert multiple estimate statements in a procedure.
When no longer needed, the array of macro variables can be
deleted through a call to the %estimate_full_free macro.
*/

/* SAS macro F.1 */

%macro estimate_full_global(
	array, /* macro variable array name */
	subject, /* variable indicating the subject */
	treatment, /* variable indicating the treatment */
	data, /* dataset name */
	ref=first /* reference treatment */
);
	%if not %length(&array) %then %do;
		%put ERROR: Value for macro parameter 'array' is missing with no default.;
		%return;
	%end;
	%else %if not %length(&subject) %then %do;
		%put ERROR: Value for macro parameter 'subject' is missing with no default.;
		%return;
	%end;
	%else %if not %length(&treatment) %then %do;
		%put ERROR: Value for macro parameter 'treatment' is missing with no default.;
		%return;
	%end;
	%else %if not %length(&data) %then %do;
		%put ERROR: Value for macro parameter 'data' is missing with no default.;
		%return;
	%end;
	%else %if not %sysfunc( exist(&data) ) %then %do;
		%put ERROR: Dataset %tslit(&data) does not exist.;
		%return;
	%end;
	%else %if not %length(&ref) %then %do;
		%put ERROR: Value for macro parameter 'ref' is missing with no default.;
		%return;
	%end;
	%global &array._s &array._t;
	%global &array._dim_s &array._dim_t;
	%let &array._s=&subject;
	%let &array._t=&treatment;
	proc sql noprint;
		select	distinct &&&array._s
				into :&array.p1 - :&array.p999999999
		from &data;
	quit;
	%if (&SQLRC ne 0) and (&SQLRC ne 4) %then %goto finish;
	%let &array._dim_s = &SqlObs; * save number of subjects ;
	proc sql noprint;
		select	distinct &&&array._t
				into :&array.t1 - :&array.t999999999
		from &data;
	quit;
	%if (&SQLRC ne 0) and (&SQLRC ne 4) %then %goto finish;
	%let &array._dim_t=&SqlObs; * save number of treatments ;
	%local i;
	%if &ref eq first %then %let i=1;
	%else %if &ref eq last %then %let i=&&&array._dim_t;
	%else %if &ref eq %sysfunc( dequote(&ref) ) %then %do;
		%put ERROR: Syntax error, expecting one of the following: a quoted string, FIRST, LAST.;
		%goto finish;
	%end;
	%else %do;
		%let ref=%sysfunc( dequote(&ref) );
		%do i=1 %to &&&array._dim_t %by 1;
			%if &&&array.t&i eq &ref %then %goto leave;
		%end;
		%leave:
		%if &i > &&&array._dim_t %then %do;
			%put ERROR: Value %tslit(&ref) not found in variable %tslit(&treatment) within dataset %tslit(&data).;
			%goto finish;
		%end;
	%end;
	%local j k m;
	%let j=&&&array.t&i;
	%do k=&i %to %eval(&&&array._dim_t-1) %by 1;
		%let m=%eval(&k+1);
		%let &array.t&k=&&&array.t&m;
	%end;
	%let &array.t&k=&j;
	%local f0 f1;
	%let k=1;
	%let i=&&&array._dim_t;
	%let f0=%sysfunc( cats(-1) );
	%do j=1 %to %eval(&i-1) %by 1;
		%local l&k;
		%let l&k=&&&array.t&j-&&&array.t&i;
		%let f1=&f0;
		%do m=&i %to %eval(&j+2) %by -1;
			%let f1=0 &f1;
		%end;
		%let f1=1 &f1;
		%do %while (&m > 2);
			%let f1=0 &f1;
			%let m=%eval(&m-1);
		%end;
		%global &array._f&k;
		%let &array._f&k=&f1;
		%let k=%eval(&k+1);
	%end;
	%do i=1 %to %eval(&&&array._dim_t-1) %by 1;
		%let f0=%sysfunc( cats(-1) );
		%do m=1 %to %eval(&i-1) %by 1;
			%let f0=0 &f0;
		%end;
		%do j=%eval(&i+1) %to %eval(&&&array._dim_t-1) %by 1;
			%local l&k;
			%let l&k=&&&array.t&j-&&&array.t&i;
			%let f1=&f0;
			%do m=&i %to %eval(&j-2) %by 1;
				%let f1=&f1 0;
			%end;
			%let f1=&f1 1;
			%do %while( &m < %eval(&&&array._dim_t-1) );
				%let f1=&f1 0;
				%let m=%eval(&m+1);
			%end;
			%global &array._f&k;
			%let &array._f&k=&f1;
			%let k=%eval(&k+1);
		%end;
	%end;
	%local s r0 r1 r2;
	%let r0=%sysfunc( cats(0) );
	%do i=2 %to &&&array._dim_s %by 1;
		%let r0=0 &r0;
	%end;
	%do s=1 %to &&&array._dim_s %by 1;
		%let r1=%sysfunc( cats(1) );
		%let r2=%sysfunc( cats(-1) );
		%do i=2 %to &s %by 1;
			%let r1=0 &r1;
			%let r2=0 &r2;
		%end;
		%do %while(&i <= &&&array._dim_s);
			%let r1=&r1. 0;
			%let r2=&r2. 0;
			%let i=%eval(&i+1);
		%end;
		%let k=1;
		%let i=&&&array._dim_t;
		%let f0=&r2;
		%do j=1 %to %eval(&i-1) %by 1;
			%global &array._l%eval(&s+(&k-1)*&&&array._dim_s);
			%let &array._l%eval(&s+(&k-1)*&&&array._dim_s) = &&l&k,&&&array.p&s;
			%let f1=&f0;
			%do m=&i %to %eval(&j+2) %by -1;
				%let f1=&r0 &f1;
			%end;
			%let f1=&r1 &f1;
			%do %while (&m > 2);
				%let f1=&r0 &f1;
				%let m=%eval(&m-1);
			%end;
			%global &array._r%eval(&s+(&k-1)*&&&array._dim_s);
			%let &array._r%eval(&s+(&k-1)*&&&array._dim_s)=&f1;
			%let k=%eval(&k+1);
		%end;
		%do i=1 %to %eval(&&&array._dim_t-1) %by 1;
			%let f0=&r2;
			%do m=1 %to %eval(&i-1) %by 1;
				%let f0=&r0 &f0;
			%end;
			%do j=%eval(&i+1) %to %eval(&&&array._dim_t-1) %by 1;
				%global &array._l%eval(&s+(&k-1)*&&&array._dim_s);
				%let &array._l%eval(&s+(&k-1)*&&&array._dim_s) = &&l&k,&&&array.p&s;
				%let f1=&f0;
				%do m=&i %to %eval(&j-2) %by 1;
					%let f1=&f1 &r0;
				%end;
				%let f1=&f1 &r1;
				%do %while( &m < %eval(&&&array._dim_t-1) );
					%let f1=&f1 &r0;
					%let m=%eval(&m+1);
				%end;
				%global &array._r%eval(&s+(&k-1)*&&&array._dim_s);
				%let &array._r%eval(&s+(&k-1)*&&&array._dim_s)=&f1;
				%let k=%eval(&k+1);
			%end;
		%end;
	%end;
	%let &array._dim_t=%eval(&k-1); * save number of treatment comparisons ;
	%return;
	%finish:
	%symdel &array._s &array._t;
	%symdel &array._dim_s &array._dim_t;
	%return;
%mend estimate_full_global;

/*
The %estimate_full macro can be used to effortlessly insert
multiple estimate statements into a procedure.
The number of estimate statements inserted equals the number
of subjects times the number of treatments minus one.
The required global macro variable array can be obtained
from a previous run of the %estimate_full_global macro.
*/

/* SAS macro F.2 */

%macro estimate_full(
	array, /* macro variable array name */
	options=, /* estimate statement options */
	random=| /* leave undefined for fixed effects models */
);
	%if not %length(&array) %then %do;
		%put ERROR: Value for macro parameter 'array' is missing with no default.;
		%abort;
	%end;
	%else %if ( %length(&random) and %superq(random) ne %str(|) ) %then %do;
		%put ERROR: Syntax error, expecting one of the following: |, missing.;
		%abort;
	%end;
	%local i j;
	%do i = 1 %to &&&array._dim_s %by 1;
		%do j = 1 %to &&&array._dim_t %by 1;
			%let k = %eval(&i+(&j-1)*&&&array._dim_s);
			estimate "&&&array._l&k" &&&array._t &&&array._f&j &random
				&&&array._s*&&&array._t &&&array._r&k / &options;
		%end;
	%end;
	%return;
%mend estimate_full;

/*
The %estimate_full_free macro is used to delete a global
array of macro variables obtained from a previous run
of the %estimate_full_global macro.
*/

/* SAS macro F.3 */

%macro estimate_full_free(
	array /* macro variable array name */
);
	%if not %length(&array) %then %do;
		%put ERROR: Value for macro parameter 'array' is missing with no default.;
		%return;
	%end;
	%symdel &array._s &array._t;
	%local i;
	%do i = 1 %to &&&array._dim_t %by 1;
		%symdel &array._f&i;
	%end;
	%local j;
	%do i = 1 %to &&&array._dim_s %by 1;
		%do j = 1 %to &&&array._dim_t %by 1;
			%symdel &array._l%eval(&i+(&j-1)*&&&array._dim_s);
			%symdel &array._r%eval(&i+(&j-1)*&&&array._dim_s);
		%end;
	%end;
	%symdel &array._dim_s &array._dim_t;
	%return;
%mend estimate_full_free;

/*
The %estimate_full_random_global macro creates a global array of
macro variables. This global array of macro variables can
later be supplied to the %estimate_full_random macro developed
to insert multiple estimate statements in a procedure.
When no longer needed, the array of macro variables can be
deleted through a call to the %estimate_full_random_free macro.
*/

/* SAS macro F.4 */

%macro estimate_full_random_global(
	array, /* macro variable array name */
	subject, /* variable indicating the subject */
	treatment, /* variable indicating the treatment */
	data, /* dataset name */
	ref=first /* reference treatment */
);
	%if not %length(&array) %then %do;
		%put ERROR: Value for macro parameter 'array' is missing with no default.;
		%return;
	%end;
	%else %if not %length(&subject) %then %do;
		%put ERROR: Value for macro parameter 'subject' is missing with no default.;
		%return;
	%end;
	%else %if not %length(&treatment) %then %do;
		%put ERROR: Value for macro parameter 'treatment' is missing with no default.;
		%return;
	%end;
	%else %if not %length(&data) %then %do;
		%put ERROR: Value for macro parameter 'data' is missing with no default.;
		%return;
	%end;
	%else %if not %sysfunc( exist(&data) ) %then %do;
		%put ERROR: Dataset %tslit(&data) does not exist.;
		%return;
	%end;
	%else %if not %length(&ref) %then %do;
		%put ERROR: Value for macro parameter 'ref' is missing with no default.;
		%return;
	%end;
	%global &array._s &array._t;
	%global &array._dim_s &array._dim_t;
	%let &array._s=&subject;
	%let &array._t=&treatment;
	proc sql noprint;
		select	distinct &&&array._s
				into :&array.p1 - :&array.p999999999
		from &data;
	quit;
	%if (&SQLRC ne 0) and (&SQLRC ne 4) %then %goto finish;
	%let &array._dim_s = &SqlObs; * save number of subjects ;
	proc sql noprint;
		select	distinct &&&array._t
				into :&array.t1 - :&array.t999999999
		from &data;
	quit;
	%if (&SQLRC ne 0) and (&SQLRC ne 4) %then %goto finish;
	%let &array._dim_t=&SqlObs; * save number of treatments ;
	%local i;
	%if &ref eq first %then %let i=1;
	%else %if &ref eq last %then %let i=&&&array._dim_t;
	%else %if &ref eq %sysfunc( dequote(&ref) ) %then %do;
		%put ERROR: Syntax error, expecting one of the following: a quoted string, FIRST, LAST.;
		%goto finish;
	%end;
	%else %do;
		%let ref=%sysfunc( dequote(&ref) );
		%do i=1 %to &&&array._dim_t %by 1;
			%if &&&array.t&i eq &ref %then %goto leave;
		%end;
		%leave:
		%if &i > &&&array._dim_t %then %do;
			%put ERROR: Value %tslit(&ref) not found in variable %tslit(&treatment) within dataset %tslit(&data).;
			%goto finish;
		%end;
	%end;
	%local j k m;
	%let j=&&&array.t&i;
	%do k=&i %to %eval(&&&array._dim_t-1) %by 1;
		%let m=%eval(&k+1);
		%let &array.t&k=&&&array.t&m;
	%end;
	%let &array.t&k=&j;
	%local f0 f1;
	%let k=1;
	%let i=&&&array._dim_t;
	%let f0=%sysfunc( cats(-1) );
	%do j=1 %to %eval(&i-1) %by 1;
		%local l&k;
		%let l&k=&&&array.t&j-&&&array.t&i;
		%let f1=&f0;
		%do m=&i %to %eval(&j+2) %by -1;
			%let f1=0 &f1;
		%end;
		%let f1=1 &f1;
		%do %while (&m > 2);
			%let f1=0 &f1;
			%let m=%eval(&m-1);
		%end;
		%global &array._f&k;
		%let &array._f&k=&f1;
		%let k=%eval(&k+1);
	%end;
	%do i=1 %to %eval(&&&array._dim_t-1) %by 1;
		%let f0=%sysfunc( cats(-1) );
		%do m=1 %to %eval(&i-1) %by 1;
			%let f0=0 &f0;
		%end;
		%do j=%eval(&i+1) %to %eval(&&&array._dim_t-1) %by 1;
			%local l&k;
			%let l&k=&&&array.t&j-&&&array.t&i;
			%let f1=&f0;
			%do m=&i %to %eval(&j-2) %by 1;
				%let f1=&f1 0;
			%end;
			%let f1=&f1 1;
			%do %while( &m < %eval(&&&array._dim_t-1) );
				%let f1=&f1 0;
				%let m=%eval(&m+1);
			%end;
			%global &array._f&k;
			%let &array._f&k=&f1;
			%let k=%eval(&k+1);
		%end;
	%end;
	%local s r;
	%let r=%sysfunc( cats(1) );
	%do s=1 %to &&&array._dim_s %by 1;
		%let k=1;
		%let i=&&&array._dim_t;
		%do j=1 %to %eval(&i-1) %by 1;
			%global &array._l%eval(&s+(&k-1)*&&&array._dim_s);
			%let &array._l%eval(&s+(&k-1)*&&&array._dim_s) = &&l&k,&&&array.p&s;
			%let k=%eval(&k+1);
		%end;
		%do i=1 %to %eval(&&&array._dim_t-1) %by 1;
			%do j=%eval(&i+1) %to %eval(&&&array._dim_t-1) %by 1;
				%global &array._l%eval(&s+(&k-1)*&&&array._dim_s);
				%let &array._l%eval(&s+(&k-1)*&&&array._dim_s) = &&l&k,&&&array.p&s;
				%let k=%eval(&k+1);
			%end;
		%end;
		%global &array._r&s;
		%let &array._r&s=&r;
		%let r=0 &r;
	%end;
	%let &array._dim_t=%eval(&k-1); * save number of treatment comparisons ;
	%return;
	%finish:
	%symdel &array._s &array._t;
	%symdel &array._dim_s &array._dim_t;
	%return;
%mend estimate_full_random_global;

/*
The %estimate_full_random macro can be used to effortlessly insert
multiple estimate statements into a procedure.
The number of estimate statements inserted equals the number
of subjects times the number of treatments minus one.
The required global macro variable array can be obtained
from a previous run of the %estimate_full_random_global macro.
*/

/* SAS macro F.5 */

%macro estimate_full_random(
	array, /* macro variable array name */
	options= /* estimate statement options */
);
	%if not %length(&array) %then %do;
		%put ERROR: Value for macro parameter 'array' is missing with no default.;
		%abort;
	%end;
	%local i j;
	%do i = 1 %to &&&array._dim_s %by 1;
		%do j = 1 %to &&&array._dim_t %by 1;
			%let k = %eval(&i+(&j-1)*&&&array._dim_s);
			estimate "&&&array._l&k" &&&array._t &&&array._f&j |
									 &&&array._t &&&array._f&j /
										subject &&&array._r&i
										&options;
		%end;
	%end;
	%return;
%mend estimate_full_random;

/*
The %estimate_full_random_free macro is used to delete a global
array of macro variables obtained from a previous run
of the %estimate_full_random_global macro.
*/

/* SAS macro F.6 */

%macro estimate_full_random_free(
	array /* macro variable array name */
);
	%if not %length(&array) %then %do;
		%put ERROR: Value for macro parameter 'array' is missing with no default.;
		%return;
	%end;
	%symdel &array._s &array._t;
	%local i;
	%do i = 1 %to &&&array._dim_t %by 1;
		%symdel &array._f&i;
	%end;
	%local j;
	%do i = 1 %to &&&array._dim_s %by 1;
		%do j = 1 %to &&&array._dim_t %by 1;
			%symdel &array._l%eval(&i+(&j-1)*&&&array._dim_s);
		%end;
		%symdel &array._r&i;
	%end;
	%symdel &array._dim_s &array._dim_t;
	%return;
%mend estimate_full_random_free;
