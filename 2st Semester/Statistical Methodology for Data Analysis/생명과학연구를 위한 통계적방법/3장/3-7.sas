data threeseven;
input first$ second$ count;
cards;
o o 794
o x 150
x o 86
x x 570
;
run;

ods select mcnemarstest;

proc freq order=data;
weight count;
tables first*second/agree;
run; quit;
