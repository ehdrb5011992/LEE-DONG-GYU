proc import 
datafile = 'C:\Users\82104\Desktop\data\7_3.csv'
out = dat7_3
dbms = csv replace;
getnames=yes;
run;

proc lifetest data=dat7_3 
    method=KM plots = (S) graphics outsurv=a;
time time*censor(0);
run; quit;
proc print data=a
