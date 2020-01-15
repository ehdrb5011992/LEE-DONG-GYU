proc import 
datafile = 'C:\Users\82104\Desktop\data\7_4.csv'
out = dat7_4
dbms = csv replace;
getnames=yes;
run;

proc lifetest data=dat7_4 plots = (S) graphics ;
time time*censor(0);
strata group;
symbol1 v=none color=black line=1;
symbol2 v=none color=black line=2;
run; quit;
