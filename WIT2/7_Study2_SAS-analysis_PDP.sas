PROC IMPORT OUT= WORK.datPDP2
            DATAFILE= "C:\data_2013\Scherer_2013\Analysis\Study2\datPDP.
txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc univariate;
var A C;
histogram;
run;

proc mixed data=datPDP2;
class Condition sub;
model A = Condition;
random int / sub=sub;
LSMEANS Condition;
run;

proc mixed data=datPDP2;
class Condition sub;
model C = Condition;
random int / sub=sub;
LSMEANS Condition;
run;

proc sort data=datPDP2;
by Condition;
run;

proc means data=datPDP2;
by Condition;
var A C;
run;

* does A vary across conditions for a particular stimulus?;
proc glm data=datPDP2;
where CueType = "hispanic";
class Condition TrialType mapping sub;
model A = Condition / ss3 effectsize alpha=.1;
*random Subject;
lsmeans Condition;
run;

* does C vary across conditions for a particular stimulus?;
proc glm data=datPDP2;
where CueType = "hispanic";
class Condition TrialType mapping sub;
model C = Condition / ss3 effectsize alpha=.1;
*random Subject;
lsmeans Condition;
run;

* get the means and sds;
proc mixed data=datPDP2;
where CueType = 'hispanic';
class Condition TrialType sub;
model A = Condition;
lsmeans Condition;
run;
quit;

proc mixed data=datPDP2;
where CueType = 'hispanic';
class Condition TrialType sub;
model C = Condition;
lsmeans Condition;
run;
quit;
