
data dat7_6;
infile 'C:\Users\82104\Desktop\data\7_6.csv' dsd firstobs=2;
input time censor age indx;
if age=1 then dum_age=1; else dum_age=0;
if indx=1 then dum_indx=1; else dum_indx=0;
run;quit;

proc phreg data=dat7_6;
model time*censor(0) = dum_age dum_indx ;
run; quit;

proc phreg data=dat7_6;
model time*censor(0) = dum_age dum_indx / selection = stepwise;
run; quit;

proc phreg data=dat7_6;
model time*censor(0) = dum_age dum_indx / selection = backward;
run; quit;

proc phreg data=dat7_6;
model time*censor(0) = dum_age dum_indx / selection = forward;
run; quit;
