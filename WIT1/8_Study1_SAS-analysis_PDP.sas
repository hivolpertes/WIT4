PROC IMPORT OUT= WORK.datPDP1 
            DATAFILE= "C:\data_2013\Scherer_2013\Analysis\Study1\datPDP.
txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

*recode trial type to four, rather than six, categories;
Data datPDP1;
Set datPDP1;
if Condition = "WIT_black_white" and prime = "Black" then NewVar = "A";
if Condition = "WIT_black_white" and prime = "White" then NewVar = "B";
if Condition = "WIT_neutral_black" and prime = "Black" then NewVar = "A";
if Condition = "WIT_neutral_black" and prime = "Neutral" then NewVar = "B";
if Condition = "WIT_neutral_white" and prime = "Neutral" then NewVar = "A"; 
if Condition = "WIT_neutral_white" and prime = "White" then NewVar = "B";
run;

proc univariate;
var A C;
histogram;
run;

proc sort data=datPDP1;
by Prime;
run;

* fecth the cell means and SEs;
proc mixed data=datPDP1;
class Condition NewVar subject;
model A = Condition|NewVar;
random int / sub=subject;
lsmeans Condition*NewVar;
run;
quit;

proc mixed data=datPDP1;
class Condition NewVar subject;
model C = Condition|NewVar;
random int / sub=subject;
lsmeans Condition*NewVar;
run;
quit;


* does A vary across conditions for a particular stimulus?;
proc mixed data=datPDP1;
by Prime;
class Condition TrialType mapping Prime Probe Subject;
model A = Condition;
*random Subject;
lsmeans Condition;
run;

* does C vary across conditions for a particular stimulus?;
proc mixed data=datPDP1;
by Prime;
class Condition TrialType mapping Prime Probe Subject;
model C = Condition;
*random Subject;
lsmeans Condition;
run;

proc mixed data=datPDP1;
class Condition TrialType mapping Prime Probe Subject;
model A = Condition*TrialType;
lsmeans Condition*TrialType;
run;
quit;

proc mixed data=datPDP1;
class Condition TrialType mapping Prime Probe Subject;
model C = Condition*TrialType;
lsmeans Condition*TrialType;
run;
quit;
