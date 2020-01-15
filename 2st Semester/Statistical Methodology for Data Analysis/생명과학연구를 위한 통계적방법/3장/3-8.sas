data threeeight;
input severity$ sex$ state$ count @@;
cards;
1 m x 2 1 m o 21 1 f x 0 1 f o 10
2 m x 2 2 m o 40 2 f x 0 2 f o 18
3 m x 6 3 m o 33 3 f x 0 3 f o 10
4 m x 17 4 m o 16 4 f x 0 4 f o 4
;
run; 
proc freq;
weight count;
tables severity*sex*state/CMH norow nocol;
run; quit;
