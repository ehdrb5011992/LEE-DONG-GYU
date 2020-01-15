data threeone;
input risk $ depr $ count @@ ;
cards;
1 1 5 1 2 21 2 1 8 2 2 82
;
run;

proc freq;
tables risk*depr / MEASURES;
weight count;
run;
