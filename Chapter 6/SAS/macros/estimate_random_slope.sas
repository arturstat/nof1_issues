/*
Written by Artur Araujo

artur.stat@gmail.com
aamarinhodearaujo1@sheffield.ac.uk

November 2019
*/

* Options for macro debugging ;
*options symbolgen mprint mlogic;

/*
The %estimate_random_slope_global macro creates a global array
of macro variables. This global array of macro variables can
later be supplied to the %estimate_random_slope macro developed
to insert multiple estimate statements in a procedure.
When no longer needed, the array of macro variables can be
deleted through a call to the %estimate_random_slope_free macro.
*/

/* SAS macro F.7 */

%macro estimate_random_slope_global(
	array, /* macro variable array name */
	subject, /* variable indicating the subject */
	treatment, /* treatment variables separated by a space character */
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
	%local i j t dataid;
	%let i = 1;
	%let dataid = %sysfunc( open(&data) );
	%if not &dataid %then %do;
		%put ERROR: Dataset %tslit(&data) cannot be opened.;
		%return;
	%end;
	%do %while(&i);
		%let t=%qscan( %superq(treatment), &i, %str( ) );
		%if not %length(&t) %then %goto leave;
		%let j = %sysfunc( varnum(&dataid, &t) );
		%if not &j %then %do;
			%put ERROR: Variable %tslit(&t) does not exist in dataset %tslit(&data).;
			%let j = %sysfunc( close(&dataid) );
			%return;
		%end;
		%if (%sysfunc( vartype(&dataid, &j) ) ne N) %then %do;
			%put ERROR: Variable %tslit(&t) is not numeric.;
			%let j = %sysfunc( close(&dataid) );
			%return;
		%end;
		proc sql noprint;
			select count(&t) into :j
			from &data
			where not (&t eq 0 or &t eq 1);
		quit;
		%if &j %then %do;
			%put ERROR: Variable %tslit(&t) is neither 0 nor 1.;
			%let j = %sysfunc( close(&dataid) );
			%return;
		%end;
		%let i = %eval(&i+1);
	%end;
	%leave:
	%let j = %sysfunc( close(&dataid) );
	%global &array._dim_s &array._dim_t;
	%let &array._dim_t = %eval(&i-1); * save number of treatment variables ;
	proc sql noprint;
		select	distinct &subject
				into :&array.p1 - :&array.p999999999
		from &data;
	quit;
	%if (&SQLRC ne 0) and (&SQLRC ne 4) %then %goto finish;
	%let &array._dim_s = &SqlObs; * save number of subjects ;
	%do i = 1 %to &&&array._dim_t %by 1;
		%global &array._t&i;
		%let &array._t&i=%qscan( %superq(treatment), &i, %str( ) );
		%let &array._t&i=%sysfunc( dequote(&&&array._t&i) );
	%end;
	%let i = 1; %let j = %sysfunc( cats(1) ); /* initialize macro variables */
	%do %until(&i >= &&&array._dim_s); /* loop through the individuals */
		%global &array._l&i &array._r&i; /* declare global macro variables */
		%let &array._l&i = &&&array.p&i; /* read individual into macro variable */
		%let &array._r&i = &j; /* read string into macro variable */
		%let j = 0 &j; /* concatenate string */
		%let i = %eval(&i+1); /* increase loop counter */
	%end;
	%global &array._l&i &array._r&i; /* declare global macro variables */
	%let &array._l&i = &&&array.p&i; /* read individual into macro variable */
	%let &array._r&i = &j; /* read string into macro variable */
	%return;
	%finish:
	%symdel &array._dim_s &array._dim_t;
	%return;
%mend estimate_random_slope_global;

/*
The %estimate_random_slope macro can be used to effortlessly
insert multiple estimate statements into a procedure.
The number of estimate statements inserted equals the number
of subjects times the number of treatment variables.
The required global macro variable array can be obtained
from a previous run of the %estimate_random_slope_global macro.
*/

/* SAS macro F.8 */

%macro estimate_random_slope(
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
			estimate "&&&array._t&j,&&&array._l&i"
						&&&array._t&j 1 | &&&array._t&j 1 /
						subject &&&array._r&i &options;
		%end;
	%end;
	%return;
%mend estimate_random_slope;

/*
The %estimate_random_slope_free macro is used to delete
a global array of macro variables obtained from a previous
run of the %estimate_random_slope_global macro.
*/

/* SAS macro F.9 */

%macro estimate_random_slope_free(
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
	%do i = 1 %to &&&array._dim_t %by 1;
		%symdel &array._t&i;
	%end;
	%symdel &array._dim_s &array._dim_t;
	%return;
%mend estimate_random_slope_free;
