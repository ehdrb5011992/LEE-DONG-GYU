data threefive;
input method$ out$ count;
cards;
s o 21
s x 2
r o  15
r x 3
;
run;

proc freq data=threefive;
weight count;
tables method*out / exact;
run; quit;
