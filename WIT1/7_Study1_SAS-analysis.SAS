/* This program is for analyzing correct-trial latencies and overall accuracy rates in the WIT Study 1.
It will probably be necessary to recode Trial Type into four, rather than six, categories in order to talk about 
the 3-way interaction justifying follow-up 2x2 ANOVAs within levels of ExperimentName.*/

PROC IMPORT OUT= WORK.Study1 
            DATAFILE= "C:\data_2013\Scherer_2013\Analysis\Study1\WIT_study1_SASdat.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

*add some columns to make analysis easier;
Data Study1;
Set Study1;
if ExperimentName = "WIT_black_white" and Cue = "Black" then NewVar = "A";
if ExperimentName = "WIT_black_white" and Cue = "White" then NewVar = "B";
if ExperimentName = "WIT_neutral_black" and Cue = "Black" then NewVar = "A";
if ExperimentName = "WIT_neutral_black" and Cue = "Neutral" then NewVar = "B";
if ExperimentName = "WIT_neutral_white" and Cue = "Neutral" then NewVar = "A"; 
if ExperimentName = "WIT_neutral_white" and Cue = "White" then NewVar = "B";
run;

*make codes for 2x2 for each prime;
data Study1;
Set Study1;
if ExperimentName = "WIT_black_white" and Cue = "Black" then OtherPrime = "White";
if ExperimentName = "WIT_black_white" and Cue = "White" then OtherPrime = "Black";
if ExperimentName = "WIT_neutral_black" and Cue = "Black" then OtherPrime = "Neutral";
if ExperimentName = "WIT_neutral_black" and Cue = "Neutral" then OtherPrime = "Black";
if ExperimentName = "WIT_neutral_white" and Cue = "Neutral" then OtherPrime = "White"; 
if ExperimentName = "WIT_neutral_white" and Cue = "White" then OtherPrime = "Neutral";
run;

proc sort data=Study1;
by ExperimentName;
run;

/* 2x2 Accuracy analyses & simple contrasts*/
PROC GLM data=Study1;
by ExperimentName;
class TrialType mapping sub Cue Probe;
model acc = Cue|Probe sub / ss3 effectsize alpha=.1;
random sub;
id sub;
MEANS Cue*Probe;
LSMEANS Cue*Probe /tdiff pdiff; *adjust=tukey;
run;
quit;

/* 2x2 RT analyses & simple contrasts*/
PROC GLM data=Study1;
by ExperimentName;
class TrialType mapping sub Cue Probe;
model rt = Cue|Probe sub / ss3 effectsize alpha=.1;
random sub;
id sub;
MEANS Cue*Probe;
*LSMEANS Cue*Probe /tdiff pdiff; *adjust=tukey;
run;
quit;

* 2x2s based on each prime (to see if the prime has changed its effect);
proc sort data=Study1;
by Cue;
run;

* for the effect size;
proc glm data=Study1;
class ExperimentName TrialType sub Cue OtherPrime Probe;
by Cue;
model acc = OtherPrime|Probe sub / e ss3 effectsize alpha=.1;
random sub;
LSMEANS OtherPrime|Probe /tdiff pdiff;
run;
quit;

* for the full 2x2 to converge;
proc mixed data=Study1;
class ExperimentName TrialType sub Cue OtherPrime Probe;
by Cue;
model acc = OtherPrime|Probe;
random int / sub=sub;
*LSMEANS OtherPrime|Probe /tdiff pdiff;
run;
quit;

*again for RT;
proc glm data=Study1;
class ExperimentName TrialType sub Cue Probe;
by Cue;
model rt = Probe ExperimentName*Probe sub / ss3 effectsize alpha=.1;
random sub;
LSMEANS ExperimentName|Probe;
run;
quit;

*here are the cell means & contrasts.
*remember that /E option is super helpful for checking that weights are assigned correctly;
*first, accuracy;
PROC MIXED data=Study1;
class ExperimentName TrialType mapping sub Cue Probe;
model acc = TrialType|Experimentname;
random int / sub=sub;
LSMEANS ExperimentName*TrialType;
/*
LSMESTIMATE ExperimentName*TrialType 'gun bias, cue Black, condition 1' 1 -1 ; 
LSMESTIMATE ExperimentName*TrialType 'gun bias, cue White, condition 1' 0 0 1 -1 ; 
LSMESTIMATE ExperimentName*TrialType 'gun bias, cue Black, condition 2' 0 0 0 0		1 -1 ; 
LSMESTIMATE ExperimentName*TrialType 'gun bias, cue Neutral, condition 2' 0 0 0 0	0 0 1 -1 ; 
LSMESTIMATE ExperimentName*TrialType 'gun bias, cue Neutral, condition 3' 0 0 0 0	0 0 0 0		1 -1 ; 
LSMESTIMATE ExperimentName*TrialType 'gun bias, cue White, condition 3' 0 0 0 0	0 0 0 0		0 0 1 -1 ; 
*/
LSMESTIMATE ExperimentName*TrialType 'does meaning of black cue change?' 1 -1 0 0	-1 1 0 0	0 0 0 0 ; 
LSMESTIMATE ExperimentName*TrialType 'does meaning of white cue change?' 0 0 1 -1 	0 0 0 0 	0 0 -1 1 ; 
LSMESTIMATE ExperimentName*TrialType 'does meaning of neutral cue change?' 0 0 0 0	0 0 1 -1	-1 1 0 0 ; 
run;
quit;

*second, reaction time;
PROC MIXED data=Study1;
class ExperimentName TrialType mapping sub Cue Probe;
model rt = TrialType|Experimentname;
random int / sub=sub;
LSMEANS ExperimentName*TrialType;
/*
LSMESTIMATE ExperimentName*TrialType 'gun bias, cue Black, condition 1' 1 -1 ; 
LSMESTIMATE ExperimentName*TrialType 'gun bias, cue White, condition 1' 0 0 1 -1 ; 
LSMESTIMATE ExperimentName*TrialType 'gun bias, cue Black, condition 2' 0 0 0 0		1 -1 ; 
LSMESTIMATE ExperimentName*TrialType 'gun bias, cue Neutral, condition 2' 0 0 0 0	0 0 1 -1 ; 
LSMESTIMATE ExperimentName*TrialType 'gun bias, cue Neutral, condition 3' 0 0 0 0	0 0 0 0		1 -1 ; 
LSMESTIMATE ExperimentName*TrialType 'gun bias, cue White, condition 3' 0 0 0 0	0 0 0 0		0 0 1 -1 ; 
*/
LSMESTIMATE ExperimentName*TrialType 'does meaning of black cue change?' 1 -1 0 0	-1 1 0 0	0 0 0 0 ; 
LSMESTIMATE ExperimentName*TrialType 'does meaning of white cue change?' 0 0 1 -1 	0 0 0 0 	0 0 -1 1 ; 
LSMESTIMATE ExperimentName*TrialType 'does meaning of neutral cue change?' 0 0 0 0	0 0 1 -1	-1 1 0 0 ; 
run;
quit;

*for the sake of completeness, the 3-way interactions;
PROC MIXED data=Study1;
class ExperimentName TrialType mapping sub Cue Probe NewVar;
model acc = Experimentname|NewVar|Probe;
random int / sub=sub;
LSMEANS ExperimentName*NewVar*Probe;
run;
quit;

PROC MIXED data=Study1;
class ExperimentName TrialType mapping sub Cue Probe NewVar;
model rt = Experimentname|NewVar|Probe;
random int / sub=sub;
LSMEANS ExperimentName*NewVar*Probe;
run;
quit;