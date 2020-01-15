data threenine;
input dia$ dis$ count ;
cards;
p o 302
p x 80
n o 179
n x 372
;
run;
proc logistic data=threenine;
class dia;
freq count;
model dis= dia / ctable pprob=(0 to 1 by .1) outroc = roc ;
run; quit;







proc freq data=threenine order = data;
weight count;
tables dia*dis ;
run;

data cal;
sen = 0.6279 ; spe = 0.8230;  prob = 0.1;
ppv = ( sen*prob ) / ( sen*prob + (1-spe)*(1-prob) ) ;
npv = ( spe*(1-prob) ) / ( spe*(1-prob) + (1-sen) * prob);
run;

title 'ppv & npv';
proc print data=cal;
var ppv npv;
run; quit;




