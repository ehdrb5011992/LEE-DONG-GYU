DATA fourfive ;
INPUT  MentalDis$ Social$ Case @@;
CARDS ;
4 1 1 4 1 9 4 1 4 4 1 3 4 0 2 4 1 0 4 0 1 4 1 3 4 1 3 4 1 7 4 0 1 4 0 2 3 1 5 3 0 6 3 1 3 3 0 1 3 1 8 3 1 2 3 0 5 3 1 5
3 1 9 3 0 3 3 1 3 3 1 1 2 0 0 2 1 4 2 0 3 2 0 9 2 1 6 2 0 4 2 0 3 1 1 8 1 1 2 1 1 7 1 0 5 1 0 4 1 0 4 1 1 8 1 0 8 1 0 9
;
run;
proc logistic order=data desc;
class Social ;
Model Mentaldis = social case / scale=None aggregate;
output out=prob pred=p;
run; quit;
proc print data=prob(obs=10);
run;

proc export data=fourfive
outfile = 'C:\Users\82104\Desktop\4_5.txt'
dbms = dlm replace;
run;
