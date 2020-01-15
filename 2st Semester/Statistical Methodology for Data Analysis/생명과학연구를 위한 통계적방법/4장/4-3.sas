data fourthree;
INPUT Lake$ Length$ Foodvar$ count @@;
CARDS ;
H 1 F 23 H 1 M 4 H 1 A 2 H 1 B 2 H 1 E 8
H 2 F 7 H 2 M 0 H 2 A 1 H 2 B 3 H 2 E 5
O 1 F 5 O 1 M 11 O 1 A 1 O 1 B 0 O 1 E 3
O 2 F 13 O 2 M 8 O 2 A 6 O 2 B 1 O 2 E 0
T 1 F 5 T 1 M 11 T 1 A 2 T 1 B 1 T 1 E 5
T 2 F 8 T 2 M 7 T 2 A 6 T 2 B 3 T 2 E 5
G 1 F 16 G 1 M 19 G 1 A 1 G 1 B 2 G 1 E 3
G 2 F 17 G 2 M 1 G 2 A 0 G 2 B 1 G 2 E 3
;
run;
proc catmod order=data;
weight count;
model Foodvar = Lake Length/ pred=prob design;
run; quit;


proc export data=fourthree
outfile = 'C:\Users\82104\Desktop\4_3.txt'
dbms = dlm replace;
run;
