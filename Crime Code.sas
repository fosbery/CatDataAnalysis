proc import datafile='N:\crime2016_chicago.xlsx'
	out=crime
	dbms=xlsx
	replace;
	getnames=yes;
run;


proc print data=crime (obs=5);
run;

proc logistic data=crime plots(only)=(roc(id=obs) effect);
class crimecat (ref='Minor Illegal Practices') / param=ref;
model crimecat = INCOMEORD / link=glogit;
run;

proc logistic data=crime;
class RACE_MAJORITY crimecat (ref='Minor Illegal Practices') / param=ref;
model crimecat = RACE_MAJORITY / link=glogit;
run;

proc logistic data=crime;
class RACE_MAJORITY crimecat (ref='Minor Illegal Practices') / param=ref;
model crimecat = INCOMEORD RACE_MAJORITY / link=glogit;
run;

proc logistic data=crime;
class RACE_MAJORITY crimecat (ref='Minor Illegal Practices') / param=ref;
model crimecat = INCOMEORD RACE_MAJORITY INCOMEORD*RACE_MAJORITY / link=glogit;
run;

proc logistic data=crime;
class RACE_MAJORITY crimecat (ref='Minor Illegal Practices');
model crimecat = MED_AGE UNEMP_PERC OPEN_SPACE_ACRES AVG_VMT_PER_100 MED_ROOMS INST_ACRES/ link=glogit selection=stepwise slentry=.01 slstay=.01 details;
run;


