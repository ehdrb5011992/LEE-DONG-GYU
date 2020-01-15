proc import 
datafile = 'C:\Users\82104\Desktop\data\7_2.csv'
out = dat7_2
dbms = csv replace;
getnames=yes;
run;

proc lifetest data=dat7_2 
    method=life
	intervals = 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 plots = (S,H)
	graphics;
time time*censor(0);
freq freq;
run; quit;

