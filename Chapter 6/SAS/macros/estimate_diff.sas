/*
Written by Artur Araujo
May 2016

Modified by Artur Araujo
April 2018
November 2019

artur.stat@gmail.com

while working for the
Luxembourg Institute of Health
on the IDEAL project

http://www.ideal.rwth-aachen.de/
*/

* Options for macro debugging ;
*options symbolgen mprint mlogic;

/*
The %estimate_diff_global macro creates a global array of
macro variables. This global array of macro variables can
later be supplied to the %estimate_diff macro developed
to insert multiple estimate statements in a procedure.
When no longer needed, the array of macro variables can be
deleted through a call to the %estimate_diff_free macro.
*/

/* SAS macro F.10 */

%macro estimate_diff_global(
	array, /* macro variable array name */
	subject, /* variable indicating the subject */
	data /* dataset name */
);
	%if not %length(&array) %then %do;
		%put ERROR: Value for macro parameter 'array' is missing with no default.;
		%return;
	%end;
	%else %if not %length(&subject) %then %do;
		%put ERROR: Value for macro parameter 'subject' is missing with no default.;
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
	%global &array._s &array._dim_s; /* declare global macro variables */
	%let &array._s = &subject; /* read variable name into macro variable */
	proc sql noprint;
		select	distinct &&&array._s
				into :&array.p1 - :&array.p999999999
		from &data; /* select unique individuals */
	quit;
	%if (&SQLRC ne 0) and (&SQLRC ne 4) %then %do;
		%symdel &array._s &array._dim_s;
		%return;
	%end;
	%let &array._dim_s = &SqlObs; /* count unique individuals */
	%local i r;
	%let i = 1; %let r = %sysfunc( cats(1) ); /* initialize macro variables */
	%do %until(&i >= &&&array._dim_s); /* loop through the individuals */
		%global &array._l&i &array._r&i; /* declare global macro variables */
		%let &array._l&i = &&&array.p&i; /* read individual into macro variable */
		%let &array._r&i = &r; /* read string into macro variable */
		%let r = 0 &r; /* concatenate string */
		%let i = %eval(&i+1); /* increase loop counter */
	%end;
	%global &array._l&i &array._r&i; /* declare global macro variables */
	%let &array._l&i = &&&array.p&i; /* read individual into macro variable */
	%let &array._r&i = &r; /* read string into macro variable */
	%return;
%mend estimate_diff_global;

/*
The %estimate_diff macro can be used to effortlessly insert
multiple estimate statements into a procedure.
It inserts as many estimate statements as there are subjects
in the dataset used to create the global array of macro
variables through a previous run of the
%estimate_diff_global macro.
*/

/* SAS macro F.11 */

%macro estimate_diff(
	array, /* macro variable array name */
	options=, /* estimate statement options */
	random=|, /* leave undefined for fixed effects models */
	noint=0 /* no intercept */
);
	%if not %length(&array) %then %do;
		%put ERROR: Value for macro parameter 'array' is missing with no default.;
		%abort;
	%end;
	%else %if ( %length(&random) and %superq(random) ne %str(|) ) %then %do;
		%put ERROR: Syntax error, expecting one of the following: |, missing.;
		%abort;
	%end;
	%else %if not %length(&noint) %then %do;
		%put ERROR: Value for macro parameter 'noint' is missing with no default.;
		%abort;
	%end;
	%local i;
	%do i = 1 %to &&&array._dim_s %by 1;
		estimate "&&&array._l&i" intercept %eval(^&noint) &random
			&&&array._s &&&array._r&i / &options;
	%end;
	%return;
%mend estimate_diff;

/*
The %estimate_diff_free macro is used to delete a global
array of macro variables obtained from a previous run
of the %estimate_diff_global macro.
*/

/* SAS macro F.12 */

%macro estimate_diff_free(
	array /* macro variable array name */
);
	%if not %length(&array) %then %do;
		%put ERROR: Value for macro parameter 'array' is missing with no default.;
		%return;
	%end;
	%local i;
	%do i = 1 %to &&&array._dim_s %by 1;
		%symdel &array._l&i &array._r&i;
	%end;
	%symdel &array._s &array._dim_s;
	%return;
%mend estimate_diff_free;

/*
The %estimate_diff_random_global macro creates a global array of
macro variables. This global array of macro variables can
later be supplied to the %estimate_diff_random macro developed
to insert multiple estimate statements in a procedure.
When no longer needed, the array of macro variables can be
deleted through a call to the %estimate_diff_random_free macro.
*/

/* SAS macro F.13 */

%macro estimate_diff_random_global(
	array, /* macro variable array name */
	subject, /* variable indicating the subject */
	data /* dataset name */
);
	%estimate_diff_global(
		array=&array,
		subject=&subject,
		data=&data
	)
	%return;
%mend estimate_diff_random_global;

/*
The %estimate_diff_random macro can be used to effortlessly
insert multiple estimate statements into a procedure.
It inserts as many estimate statements as there are subjects
in the dataset used to create the global array of macro
variables through a previous run of the
%estimate_diff_random_global macro.
*/

/* SAS macro F.14 */

%macro estimate_diff_random(
	array, /* macro variable array name */
	options= /* estimate statement options */
);
	%if not %length(&array) %then %do;
		%put ERROR: Value for macro parameter 'array' is missing with no default.;
		%abort;
	%end;
	%local i;
	%do i = 1 %to &&&array._dim_s %by 1;
		estimate "&&&array._l&i" intercept 1 |
								 intercept 1 /
									subject &&&array._r&i
									&options;
	%end;
	%return;
%mend estimate_diff_random;

/*
The %estimate_diff_random_free macro is used to delete
a global array of macro variables obtained from a
previous run of the %estimate_diff_random_global macro.
*/

/* SAS macro F.15 */

%macro estimate_diff_random_free(
	array /* macro variable array name */
);
	%estimate_diff_free(array=&array)
	%return;
%mend estimate_diff_random_free;
