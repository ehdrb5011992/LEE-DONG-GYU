/*데이터 불러오기*/
data dat7_2;
infile 'C:\Users\82104\Desktop\cox_regression.csv' dsd firstobs=2;
input no time censor aaha pe fs lvdd wmi group ;
run;quit;


proc phreg data=dat7_2;
model time*censor(0) = aaha pe fs lvdd wmi  ;
run; quit;

/*변수 선택*/
proc phreg data=dat7_2;
model time*censor(0) = aaha pe fs lvdd wmi  / selection = stepwise sle=0.3 sls=0.3;
run; quit;


/*비례위험모형 검정- 평행선 검정*/
proc phreg data=dat7_2;
model time*censor(0) = wmi fs  ;
strata group;
baseline out=c survival= s loglogs=lls;
run; quit;

axis1 order=(30 to 60 by 10) ;

proc gplot data=c;
plot lls*time=group / haxis = axis1; 
symbol1 interpol=join color=black line=1;
symbol2 interpol=join color=black line=2;
run; quit;
