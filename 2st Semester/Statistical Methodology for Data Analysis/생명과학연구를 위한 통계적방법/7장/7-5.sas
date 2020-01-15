proc import 
datafile = 'C:\Users\82104\Desktop\data\7_5.csv'
out = dat7_5
dbms = csv replace;
getnames=yes;
run;

/*Exp , Weibull*/
proc lifetest data=dat7_5 plots=(LS,LLS) graphics ;
time time*censor(0);
run;
/*Exp , Weibull End */


/*Log-Normal , Log-logisitic */
proc lifetest data=dat7_5 outsurv = a;
time time*censor(0);

data b; set a;
s=survival; 
logit = log((1-S)/S);
lnorm = probit(1-S);
ltime=log(time);

proc gplot; 
symbol1 value=none i=join;
plot logit*ltime lnorm*ltime;
run; quit;
/*Log-Normal , Log-logisitic End*/



/*7-5-(2) model comparison*/
proc lifereg data=dat7_5;
class smoke;
model time*censor(0) = / dist=weibull;
run; quit;


proc lifereg data=dat7_5;
class smoke;
model time*censor(0) = age BMI start_age smoke / dist=weibull;
run; quit;

