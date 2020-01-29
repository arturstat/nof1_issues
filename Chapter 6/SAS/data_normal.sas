/*
Written by Artur Araujo
October 2019

artur.stat@gmail.com

while working
on the IDEAL project

http://www.ideal.rwth-aachen.de/

These data were kindly supplied by Dr. Michael Yelland.
Yelland MJ, Poulos CJ, Pillans PI, et al.
N-of-1 randomized trials to assess the efficacy of gabapentin for chronic neuropathic pain.
Pain medicine 2009; 10: 754-761. DOI: 10.1111/j.1526-4637.2009.00615.x.
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

/* SAS program E.1 */

data WORK.gaba_placebo_raw;
	input
		id rfs
		pain_p1-pain_p3 pain_g1-pain_g3
		sleep_p1-sleep_p3 sleep_g1-sleep_g3
		func_p1-func_p3 func_g1-func_g3;
	datalines;
		751 1 3.29 3.14 . 0 . . 0.14 3.14 . 0 . . 3.4 9.4 . 0 . .
		752 1 6.14 . . 3.86 . . 9 . . 9 . . 8.2 . . 6.6 . .
		768 0 1.43 3.5 2.57 3.33 0 3.43 2.86 2.86 1 3.14 2.29 4.71 5 0 0 0 0 0
		772 1 4.57 2.71 4 4.14 4.86 2.71 3.57 1.71 3 2.43 2.86 2 9.5 8.67 . 9 8.5 8
		773 1 8 6 6 8 6 6 5.43 6 6 6.29 6 6 8 7 . 8 7 6
		804 0 0 3 2.86 2.71 2.57 7.57 0.86 3.43 2.86 1.14 2.43 7.71 4 5.67 . 3.33 . .
		805 1 4.57 5.14 6 3.57 3.71 3.14 4.14 5.86 6.29 3.71 3.57 2.86 5 6 5.67 4.66 4 4
		806 1 7.2 4.14 3.57 6 8.57 7.71 6.6 4 2.86 5.17 8.14 7.43 5 4 3 6.5 8.5 8.5
		807 1 3.43 4 5.29 1 1.5 3.14 3.43 4.86 5.14 1.86 2 2.14 3 3.75 6.33 1.67 3 3.33
		808 1 1 2.67 4.57 2.14 2.57 7.17 0 1.8 2.71 2 1.17 5.67 0.67 2 7 2 6 7
		809 1 4.33 4.43 2 2.86 1 0.71 0.2 1.2 1.83 1 0.14 0.29 3.5 2.67 10 2.67 1.33 1
		811 1 2.57 5 3.14 3 2.14 2.43 0 2.67 0.71 0 0.71 0 3.5 4 3.5 3.5 3.75 2.5
		812 1 6.43 3.86 2.57 5.29 3.86 2 6.29 4 2.57 4.29 4 2 4.5 . 4 3.5 3.5 2
		813 1 5.43 5.5 5.57 5.86 5.57 5.57 4.86 4.4 4.29 4.86 4.29 4.67 5.67 4.67 . 5.33 4.33 5
		828 0 2.71 1.43 3 2 2 2.71 0 0 0 0 0 0 4.6 3.2 4 3.4 2.5 4.6
		829 0 3.71 3.29 3.14 1.57 2 3.57 5.71 5.29 4.71 3 3.86 4.71 2.5 3.25 3.75 7.75 3.5 3.75
		831 1 6.83 8.43 8.57 5.57 4.14 4.86 5.5 7.71 7.86 3.71 3.43 4.14 6.67 8 8 3.67 3.67 4
		832 1 10 8.71 7.14 8.29 7 6.57 8.5 5.29 4.57 4.86 4.86 4.29 . 6.67 4 5.33 5.33 4
		833 1 6.57 8.17 7.86 3.57 1.29 1.29 8 9.33 9.29 4.57 1.57 1.29 7 7.67 6.33 2.33 1 4
		850 1 6.29 3.86 2.29 3.71 2.2 0.29 0 1.71 0.29 0 0 0.29 8.2 7 1.6 0 0.8 1
		851 1 7.86 6.57 6.86 7.29 6.67 7.57 7.43 6.14 5.67 6.5 4.83 5.71 8 7.8 7.8 7.6 7.2 7.4
		852 1 4.33 2 2 4 2 2 5 0 0 . 0 0 8 6.2 3.4 1 5 2
		853 1 6 6.14 6 6.57 5 5 5.5 5 6 6.14 4.29 4 6.2 6.2 6.2 7.6 6.4 5.8
		862 1 4 3 3.29 4.67 4.57 5.29 2.43 2.86 2 2.33 3.71 4.14 . 5 5.75 9.25 5 6.5
		863 1 5.71 5.71 7 6 6 7 9 9 9 9 9 9.29 7.6 8 7.8 7.8 8 7.8
		864 1 6.43 5.86 . 6.17 5.71 . 5.86 5.57 . 5.33 5.57 . 5.4 4.4 . 4.8 5.2 .
		865 1 5.83 7.29 7.57 5.57 6.33 3.43 4.33 5.83 6 3.71 4.29 2 3.4 5 5.4 3.2 6 3.8
		866 1 6.86 5.57 2.14 6.71 3.71 7 7 5.43 1.14 6.14 3.14 6.29 8.6 6 2.4 6.8 3.6 4.8
		867 1 3.14 4.14 4 5.57 3.71 4.14 1.43 1.71 1.14 2.57 1.29 1.71 5.6 6.4 6.6 6.4 7 6.4
		869 1 3.71 4 2 1.14 1.29 2.14 3.86 3.86 1.57 1.43 0.86 1.86 6.2 5.6 4.8 1.6 2.4 5
		870 1 3.29 3.57 4.57 3.29 4.14 4.86 3.14 1.14 2 7 1.86 2 3.4 4 4.2 5 5 4
		898 1 4.57 1.43 5 2.86 8 3.71 5.71 2.14 5.57 3.86 9 4.71 6.4 6.6 7 5.2 8.6 7.4
		900 1 7.14 6.29 6.43 5.57 6 7.14 7 6.43 6.57 6 5.86 6.57 7.6 7.2 7.6 4.6 6 7.6
		922 1 7.29 8 7.57 4.57 6.57 3.43 0 0 0 0 0 0 7.2 8 8 5 6.4 3.6
		923 1 5.57 4.57 . 3.43 4.43 . 5.14 2.14 . 2.86 2.43 . 2.6 7.4 . 4 8.4 .
		924 1 4.14 3.5 6.6 5 2.43 4.86 4.14 3 4.8 4 2.71 3.43 4.2 6.2 . 5.8 5.2 5.2
		925 1 2.57 1 7 2.43 3.43 1.71 1.14 0.86 4.71 1.29 2.14 0.86 3 2.2 7.4 3.2 3.4 2
		926 1 7.71 6.4 9.14 8.14 5.71 8.43 3.29 1.4 5 5.71 2.57 3.57 7 8.6 7 7.6 6.4 8.4
		927 1 0.29 1.43 2 0.71 1.43 1.43 0.43 0 0.71 0 0.43 0.71 2.2 4.4 2.6 2.2 4.2 4
		928 1 0.86 1.57 1.29 1 0.17 0.17 0 0 0 0 0 0 1.6 0.8 0.2 1.8 0 0
		929 1 6.14 1.14 3.43 1.33 6.17 2.29 3 0.14 0.57 0 1.5 0 6.4 4 4 6 5.6 3.8
		931 1 2.5 5.14 6 7.29 1 4.43 6.5 6.4 4.25 6.71 7 6.29 . 7.6 4.8 10 . .
		932 1 4 1.33 5.29 0.71 2 0 0 0 0 0 0 0 3 2 8 3.4 4 1.5
		934 1 5.57 5.86 5.29 5.29 4.57 4.29 3.57 2.71 2.86 2.43 1.71 2 5.8 5.2 5.6 6.6 3.8 4.4
		935 1 1.71 0.6 3.6 2.57 2.14 0.2 0.29 0.2 0.86 1.14 1.14 0 1.33 3 1.75 0 6.33 1.4
		936 1 6.43 . . 6 . . 8.43 . . 9 . . 9.2 . . 10 . .
	;
run;

/* SAS program E.2 */

%let CycleNumber=3;
%let treatA=Placebo;
%let treatB=Gabapentin;

* Convert dataset with one observation per subject
to a dataset with several observations per subject ;
%macro sqlBody001;
	%local i;
	%do i=2 %to &CycleNumber %by 1;
		union
		select	id,
				rfs,
	 			&treat as Treatment,
				&i as Cycle,
				pain_&var.&i as Pain,
				sleep_&var.&i as Sleep,
				func_&var.&i as Function
		from WORK.gaba_placebo_raw
	%end;
%mend sqlBody001;

proc sql;
	%let treat="&treatA";
	%let var=p;
	create view A as
	select	id,
			rfs,
			&treat as Treatment,
			1 as Cycle,
			pain_&var.1 as Pain,
			sleep_&var.1 as Sleep,
			func_&var.1 as Function
	from WORK.gaba_placebo_raw
	%sqlBody001
	;
	%let treat="&treatB";
	%let var=g;
	create view B as
	select	id,
			rfs,
			&treat as Treatment,
			1 as Cycle,
			pain_&var.1 as Pain,
			sleep_&var.1 as Sleep,
			func_&var.1 as Function
	from WORK.gaba_placebo_raw
	%sqlBody001
	;
	create table WORK.gaba_placebo as
	select * from A
	union
	select * from B
	order by id, Cycle;
	drop view A, B;
quit;

%symdel CycleNumber;
%symdel treat var;

* Delete macros from WORK.Sasmacr ;
proc catalog cat=WORK.Sasmacr;
	delete sqlBody001 / et=macro;
quit;

/* SAS program E.3 */

* Add Period variable to dataset ;
data WORK.gaba_placebo;
	set WORK.gaba_placebo;
	by id;
	if first.id then Period=1;
	else Period+1;
run;

* Reorder variables in dataset ;
data WORK.gaba_placebo;
	retain id rfs Treatment Cycle Period;
	set WORK.gaba_placebo;
run;

* Simulate random order of treatment administration ;
%let seed=314159;

proc iml;
	varNames={"id" "rfs" "Treatment" "Cycle" "Period" "Pain" "Sleep" "Function"};
	use WORK.gaba_placebo;
	read all var varNames; * read variables into vectors ;
	close WORK.gaba_placebo;
	uID=unique(id); * get unique subjects ;
	uCycle=unique(Cycle); * get unique cycles ;
	call randseed(&seed); * set seed for RNG;
	do i=1 to ncol(uID) by 1; * loop through the subjects ;
		do j=1 to ncol(uCycle) by 1; * loop through the cycles ;
			index=loc(id=uID[i] & Cycle=uCycle[j]); * locate subject and cycle ;
			call randgen(x, "bernoulli", 0.5); * toss the coin ;
			if x then do; * if true then switch order of treatments ;
				p1=Period[index[1]];
				Period[index[1]]=Period[index[2]];
				Period[index[2]]=p1;
			end;
		end;
	end;
	create WORK.gaba_placebo var varNames;
	append;
	close WORK.gaba_placebo;
quit;

%symdel seed;

* Order dataset by id, Cycle and Period ;
proc sort data=WORK.gaba_placebo;
	by id Cycle Period;
run;

/* SAS program E.4 */

* Create new dataset by differencing the outcome
variable under &treatB and under &treatA
while taking old dataset as input ;
data WORK.gaba_placebo_diff;
	merge
		WORK.gaba_placebo(
			where=(Treatment="&treatA")
			rename=(
				Period=Period_&treatA
				Pain=Pain_&treatA
				Sleep=Sleep_&treatA
				Function=Function_&treatA
			)
		)
		WORK.gaba_placebo(
			where=(Treatment="&treatB")
			rename=(
				Period=Period_&treatB
				Pain=Pain_&treatB
				Sleep=Sleep_&treatB
				Function=Function_&treatB
			)
		);
	by id Cycle;
	deltaPeriod=Period_&treatB-Period_&treatA;
	deltaPain=Pain_&treatB-Pain_&treatA;
	deltaSleep=Sleep_&treatB-Sleep_&treatA;
	deltaFunction=Function_&treatB-Function_&treatA;
	drop Treatment Period_&treatA Period_&treatB;
run;

* Order dataset by id and Cycle ;
proc sort data=WORK.gaba_placebo_diff;
	by id Cycle;
run;

%symdel treatA treatB;

/* SAS program E.5 */

* Remove missing observations ;
proc sql noprint;
	create table WORK.gaba_placebo_miss as
	select	id,
			Treatment,
			Cycle,
			Period,
			Pain
	from WORK.gaba_placebo
	where Pain is not null;
quit;
