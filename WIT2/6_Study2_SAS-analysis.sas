*DON'T FORGET THAT THE DATA SET INCLUDES NON-WHITE SUBJECTS!;

PROC IMPORT OUT= WORK.dat 
            DATAFILE= "C:\data_2013\Scherer_2013\Analysis\Study2\WIT_study2_SASdat.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC IMPORT OUT= WORK.dat_nozim
            DATAFILE= "C:\data_2013\Scherer_2013\Analysis\Study2\WIT_study2_SASdat_nozim.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/*
*old code from study1 -- one way I might consider doing it;
PROC ANOVA data=accdat;
class Condition TrialSort;
model Probe_Acc = Condition|TrialSort;
run;
quit;
*/

/*
*old code from study1 -- although I don't think LSMESTIMATE works!;
PROC GLM data=accdat;
class Condition TrialSort;
model Probe_Acc = Condition|TrialSort;
LSMEANS Condition|TrialSort;
LSMESTIMATE Condition|TrialSort 'Hisp bias, black & hispanic faces' 	1 -1;
LSMESTIMATE Condition|TrialSort 'Hisp bias, white & hispanic faces' 	0 0 0 0  1 -1;
LSMESTIMATE Condition|TrialSort 'Hisp bias, neutral & hispanic faces' 	0 0 0 0  0 0 0 0 1 -1;
run;*/

*get everything sorted;
proc sort data=dat;
	by Condition;
run;
proc sort data=dat_nozim;
	by Condition;
run;	
data dat_2; set dat;
	TrialSort = TrialType;
	if TrialType in ("BlackGun", "WhiteGun", "NeutGun", "WhiteGun") then TrialSort = "OtherGun";
	if TrialType in ("BlackTool", "WhiteTool", "NeutTool", "WhiteTool") then TrialSort = "OtherTool";
if trialsort in ("HispTool", "HispGun") then CueSort = "Hispanic";
if trialsort in ("OtherTool", "OtherGun") then CueSort = "Other";
run; 
data dat_nozim2; set dat_nozim;
	TrialSort = TrialType;
	if TrialType in ("BlackGun", "WhiteGun", "NeutGun", "WhiteGun") then TrialSort = "OtherGun";
	if TrialType in ("BlackTool", "WhiteTool", "NeutTool", "WhiteTool") then TrialSort = "OtherTool";
if trialsort in ("HispTool", "HispGun") then CueSort = "Hispanic";
if trialsort in ("OtherTool", "OtherGun") then CueSort = "Other";
run; 

/*
NOW THE ANALYSES BEGIN!
#JBH_3way for the 3x3 proc mixed
#JBH_Hisp for 2x3 where prime=Hisp
#JBHMIXED for overall proc mixed
#JBH2x2 for BY: CONDITION proc mixed
#JBH2x2GLM for BY:CONDITION proc GLM with pairwise contrasts
#JBHMIXED02 for comparison of bias across conditions
Skip to #JBHGRAPH for graph output
*/

*#JBH_3WAY;
*Accuracy first;
proc mixed data=dat_2;
where race = "white";
class Condition CueSort ProbeType;
model acc = Condition|CueSort|ProbeType;
random int / sub=sub;
lsmeans Condition|CueSort|ProbeType;
run;

*then reaction times;
proc mixed data=dat_2;
where race = "white";
class Condition CueSort ProbeType;
model rt = Condition|CueSort|ProbeType;
random int / sub=sub;
lsmeans Condition|CueSort|ProbeType;
run;

*#JBH_HISP
*Accuracy first;
proc glm data=dat_2;
where race = "white" & CueSort = "Hispanic";
class sub Condition CueSort ProbeType;
model acc = ProbeType Condition*ProbeType sub/ ss3 effectsize alpha=.1;
random sub;
lsmeans Condition|ProbeType;
run;
quit;

*then reaction times;
proc glm data=dat_2;
where race = "white" & CueSort = "Hispanic";
class sub Condition ProbeType;
model rt = ProbeType|Condition sub/ ss3 effectsize alpha=.1;
random sub;
lsmeans Condition|ProbeType;
run;
quit;

*#JBH2x2GLM;
*Accuracy first;
proc GLM data=dat_2;
where race = "white";
by Condition;
class CueType ProbeType sub;
model acc = CueType|ProbeType sub / ss3 effectsize alpha=.1;
random sub;
id sub;
*lsmeans CueType*ProbeType /tdiff pdiff;
*means CueType*ProbeType;
run;

*then reaction time;
proc GLM data=dat_2;
where race = "white";
by Condition;
class CueType ProbeType sub;
model rt = CueType|ProbeType sub / ss3 effectsize alpha=.1;
random sub;
id sub;
*lsmeans CueType*ProbeType /tdiff pdiff;
*means CueType*ProbeType;
run;

*JBHMIXED;
*now i just need to figure out what these contrasts were;
*ACCURACY first;
PROC mixed data=dat_2;
where race = "white";
class Condition TrialSort sub;
model acc /*rt*/ = Condition|TrialSort;
random int/ sub=sub;
LSMEANS Condition|TrialSort;
/*LSMESTIMATE Condition	'whitehisp vs others' -1 2 -1;
LSMESTIMATE Condition	'whitehisp vs blackhisp' -1 1;
LSMESTIMATE Condition	'whitehisp vs neuhisp' 0 1 -1;
LSMESTIMATE Condition 'blackhisp vs neuthisp' 1 0 -1; 
LSMESTIMATE TrialSort 'hispgun vs hisptool' 1 -1;
LSMESTIMATE TrialSort 'othergun vs othertool' 0 0 1 -1;
LSMESTIMATE Condition*TrialSort 'others contrast, blackhisp'			0 0 1 -1;
LSMESTIMATE Condition*TrialSort 'others contrast, whitehisp'			0 0 0 0 	0 0 1 -1;
LSMESTIMATE Condition*TrialSort 'others contrast, neuthisp'				0 0 0 0		0 0 0 0 	0 0 1 -1;
LSMESTIMATE Condition*TrialSort 'Hisp bias, BlackHisp Condition' 		1 -1;
LSMESTIMATE Condition*TrialSort 'Hisp bias, WhiteHisp Condition' 		0 0 0 0   1 -1;
LSMESTIMATE Condition*TrialSort 'Hisp bias, NeuHisp Condition' 			0 0 0 0   0 0 0 0  1 -1;
LSMESTIMATE Condition*TrialSort 'gun reduced when black & hispanic?'	2 0 0 0  -1 0 0 0 -1 0 0 0;  
LSMESTIMATE Condition*TrialSort 'tool increased when black & hispanic?' 0 2 0 0  0 -1 0 0 0 -1 0 0;
LSMESTIMATE Condition*TrialSort 'interaction when black & hispanic?'	2 -2 0 0  -1 1 0 0  -1 1 0 0;
*/
LSMESTIMATE Condition*TrialSort '1 vs 2 Hisp-Gun Bias'	1 -1 0 0	-1 1 0 0	0 0 0 0 /e;
LSMESTIMATE Condition*TrialSort '1 vs 3 Hisp-Gun Bias'	1 -1 0 0 	 0 0 0 0	-1 1 0 0 /e;
LSMESTIMATE Condition*TrialSort '2 vs 3 Hisp-Gun Bias'	0  0 0 0	1 -1 0 0	-1 1 0 0 /e;
run;

*Now reaction time;
PROC mixed data=dat_2;
where race = "white";
class Condition TrialSort sub;
model rt = Condition|TrialSort;
random int/ sub=sub;
LSMEANS Condition|TrialSort;
/*LSMESTIMATE Condition	'whitehisp vs others' -1 2 -1;
LSMESTIMATE Condition	'whitehisp vs blackhisp' -1 1;
LSMESTIMATE Condition	'whitehisp vs neuhisp' 0 1 -1;
LSMESTIMATE Condition 'blackhisp vs neuthisp' 1 0 -1; 
LSMESTIMATE TrialSort 'hispgun vs hisptool' 1 -1;
LSMESTIMATE TrialSort 'othergun vs othertool' 0 0 1 -1;
LSMESTIMATE Condition*TrialSort 'others contrast, blackhisp'			0 0 1 -1;
LSMESTIMATE Condition*TrialSort 'others contrast, whitehisp'			0 0 0 0 	0 0 1 -1;
LSMESTIMATE Condition*TrialSort 'others contrast, neuthisp'				0 0 0 0		0 0 0 0 	0 0 1 -1;
LSMESTIMATE Condition*TrialSort 'Hisp bias, BlackHisp Condition' 		1 -1;
LSMESTIMATE Condition*TrialSort 'Hisp bias, WhiteHisp Condition' 		0 0 0 0   1 -1;
LSMESTIMATE Condition*TrialSort 'Hisp bias, NeuHisp Condition' 			0 0 0 0   0 0 0 0  1 -1;
LSMESTIMATE Condition*TrialSort 'gun reduced when black & hispanic?'	2 0 0 0  -1 0 0 0 -1 0 0 0;  
LSMESTIMATE Condition*TrialSort 'tool increased when black & hispanic?' 0 2 0 0  0 -1 0 0 0 -1 0 0;
LSMESTIMATE Condition*TrialSort 'interaction when black & hispanic?'	2 -2 0 0  -1 1 0 0  -1 1 0 0;
*/
LSMESTIMATE Condition*TrialSort '1 vs 2 Hisp-Gun Bias'	1 -1 0 0	-1 1 0 0	0 0 0 0 /e;
LSMESTIMATE Condition*TrialSort '1 vs 3 Hisp-Gun Bias'	1 -1 0 0 	 0 0 0 0	-1 1 0 0 /e;
LSMESTIMATE Condition*TrialSort '2 vs 3 Hisp-Gun Bias'	0  0 0 0	1 -1 0 0	-1 1 0 0 /e;
run;


*p-values a wee bit better in nozim but I think that's just chance;
*before writing this up I recommend appending /e to all contrasts to make super-certain.;
*#JBHMIXED02;
proc mixed data=dat_2 /*plots(only)=(
					boxplot(fixed)
					boxplot(observed)
				)*/;
where race = "white";
class Condition cuesort probetype sub;
model acc /*rt*/ = Condition|cuesort|probetype;
random int / sub=sub;
*condition black, white, neutral;
*cue hispanic, other;
*probe weapon, tool;
LSMESTIMATE Condition*CueSort*ProbeType 'condition 3 hisp-bias vs cond 2 hisp-bias' 
[-1, 3 1 1] [1, 3 1 2]
[1, 2 1 1] [-1, 2 1 2];
LSMESTIMATE Condition*CueSort*ProbeType 'condition 2 hisp-bias vs cond 1 hisp-bias' 
[-1, 2 1 1] [1, 2 1 2]
[1, 1 1 1] [-1, 1 1 2]; 
LSMESTIMATE Condition*CueSort*ProbeType 'condition 3 hisp-bias vs cond 1 hisp-bias' 
[-1, 3 1 1] [1, 3 1 2]
[1, 1 1 1] [-1, 1 1 2];
LSMEANS Condition|CueSort|ProbeType;
run;

*now RT data;
PROC mixed data=dat_2;
where race = "white";
class Condition TrialSort;
model rt = Condition|TrialSort;
random int/ sub=sub;
LSMEANS Condition|TrialSort;
/*LSMESTIMATE Condition	'whitehisp vs others' -1 2 -1;
LSMESTIMATE Condition	'whitehisp vs blackhisp' -1 1;
LSMESTIMATE Condition	'whitehisp vs neuhisp' 0 1 -1;
LSMESTIMATE Condition 'blackhisp vs neuthisp' 1 0 -1; */
LSMESTIMATE TrialSort 'hispgun vs hisptool' 1 -1;
LSMESTIMATE TrialSort 'othergun vs othertool' 0 0 1 -1;
LSMESTIMATE Condition*TrialSort 'others contrast, blackhisp'			0 0 1 -1;
LSMESTIMATE Condition*TrialSort 'others contrast, whitehisp'			0 0 0 0 	0 0 1 -1;
LSMESTIMATE Condition*TrialSort 'others contrast, neuthisp'				0 0 0 0		0 0 0 0 	0 0 1 -1;
LSMESTIMATE Condition*TrialSort 'Hisp bias, BlackHisp Condition' 		1 -1;
LSMESTIMATE Condition*TrialSort 'Hisp bias, WhiteHisp Condition' 		0 0 0 0   1 -1;
LSMESTIMATE Condition*TrialSort 'Hisp bias, NeuHisp Condition' 			0 0 0 0   0 0 0 0  1 -1;
LSMESTIMATE Condition*TrialSort 'gun reduced when black & hispanic?'	2 0 0 0  -1 0 0 0 -1 0 0 0;  
LSMESTIMATE Condition*TrialSort 'tool increased when black & hispanic?' 0 2 0 0  0 -1 0 0 0 -1 0 0;
LSMESTIMATE Condition*TrialSort 'interaction when black & hispanic?'	2 -2 0 0  -1 1 0 0  -1 1 0 0;
run;

proc mixed data=dat_2 /*plots(only)=(
					boxplot(fixed)
					boxplot(observed)
				)*/;
where race = "white";
class Condition cuesort probetype;
model rt = Condition|cuesort|probetype;
random int / sub=sub;
*condition black, white, neutral;
*cue hispanic, other;
*probe weapon, tool;
LSMESTIMATE Condition*CueSort*ProbeType 'condition 3 hisp-bias vs cond 2 hisp-bias' 
[-1, 3 1 1] [1, 3 1 2]
[1, 2 1 1] [-1, 2 1 2] ;
LSMESTIMATE Condition*CueSort*ProbeType 'condition 2 hisp-bias vs cond 1 hisp-bias' 
[-1, 2 1 1] [1, 2 1 2]
[1, 1 1 1] [-1, 1 1 2] ; 
LSMESTIMATE Condition*CueSort*ProbeType 'condition 3 hisp-bias vs cond 1 hisp-bias' 
[-1, 3 1 1] [1, 3 1 2]
[1, 1 1 1] [-1, 1 1 2] ;
run;



* #JBH2x2;
ods graphics on;
proc mixed data=dat_2;
where race="white";
class Condition CueSort ProbeType;
by Condition;
model acc = CueSort|ProbeType;
random int / sub=sub;
LSMEANS CueSort|ProbeType;
LSMESTIMATE cueSort*probeType 'hispanic gun-bias' 	-1 1;
LSMESTIMATE cueSort*probeType 'other tool-bias'	  	0 0 1 -1;
LSMESTIMATE cueSort*probeType 'gun acc (+hisp)'		0 1 0 -1;
LSMESTIMATE cueSort*probeType 'tool acc (+hisp)'	1 0 -1 0;
run;

proc mixed data=dat_2;
where race="white";
class Condition CueSort ProbeType;
by Condition;
model rt = CueSort|ProbeType;
random int / sub=sub;
LSMEANS CueSort|ProbeType;
LSMESTIMATE cueSort*probeType 'hispanic gun-bias' 	-1 1;
LSMESTIMATE cueSort*probeType 'other tool-bias'	  	0 0 1 -1;
LSMESTIMATE cueSort*probeType 'gun acc (+hisp)'		0 1 0 -1;
LSMESTIMATE cueSort*probeType 'tool acc (+hisp)'	1 0 -1 0;
run;


*#JBHGRAPH;
*For overall graphs for convenient mean comparisons;
ods graphics on;
ods output rtf;
title1 'Scherer Study 2';
title2 'with all hispanic stimuli';
PROC GLM data=dat_2;
where race = "white";
class Condition TrialSort sub CueType;
id sub;
model acc rt = Condition|Trialsort sub;
random sub;
means Condition*Trialsort;
run;
quit;

proc boxplot data=dat_2;
by Condition;
ID sub;
PLOT acc * TrialType;
insetgroup 
run;


title1 'Scherer Study 2';
title2 'minus that one hispanic face';
PROC GLM data=dat_nozim2;
where race = "white";
class Condition TrialSort sub CueType;
id sub;
model acc rt = Condition|Trialsort sub;
random sub;
means Condition*Trialsort;
run;
quit;

*One final peek to see what changing effects look like for JUST THE HISPANIC PRIMES;
*Once for accuracy...;
PROC MIXED data=dat_2;
where race = "white" and CueType="hispanic";
class Condition ProbeType;
model acc = Condition|ProbeType;
random int /sub=sub;
lsmeans Condition|ProbeType;
*lsmestimate Condition*ProbeType ';
run;

*And again for reaction time...;
PROC MIXED data=dat_2;
where race = "white" and CueType="hispanic";
class Condition ProbeType;
model rt = Condition ProbeType Condition*ProbeType;
random int /sub=sub;
lsmeans Condition*Probetype;
*lsmestimate ';
run;

proc GLM data=dat_2;
where race = "white" and CueType="hispanic";
class Condition ProbeType sub;
model acc rt = ProbeType Condition Condition*ProbeType sub;
random sub;
lsmeans Condition*ProbeType /tdiff pdiff;
*lsmestimate Condition*ProbeType ';
run;

*let's run the contrasts.;
proc mixed data=dat_2;
where race="white" and CueType="hispanic";
class Condition ProbeType sub;
model acc = ProbeType Condition Condition*ProbeType;
random int / sub=sub;
lsmeans Condition*Probetype;
lsmestimate Condition*Probetype 
'Neu/Hisp bias greater than White/Hisp?' 0 0 1 -1 -1 1;
lsmestimate Condition*Probetype 
'Neu/Hisp bias greater than Black/Hisp?' 1 -1 0 0 -1 1;
lsmestimate Condition*Probetype 
'White/Hisp bias greater than Black/Hisp?' 1 -1 -1 1 0 0;
run;
quit;


*let's run the contrasts.;
proc mixed data=dat_2;
where race="white" and CueType="hispanic";
class Condition ProbeType sub;
model rt = ProbeType Condition Condition*ProbeType;
random int / sub=sub;
lsmeans Condition*Probetype;
lsmestimate Condition*Probetype 
'Neu/Hisp bias greater than White/Hisp?' 0 0 1 -1 -1 1;
lsmestimate Condition*Probetype 
'Neu/Hisp bias greater than Black/Hisp?' 1 -1 0 0 -1 1;
lsmestimate Condition*Probetype 
'White/Hisp bias greater than Black/Hisp?' 1 -1 -1 1 0 0;
run;
quit;
