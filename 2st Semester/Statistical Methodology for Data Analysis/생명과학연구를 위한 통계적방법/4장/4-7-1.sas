DATA fourseven;
INPUT Seatbelt$ Accident$ Death$ count @@;
CARDS ;
Y Y Y 659 Y Y N 270 Y N Y 532 Y N N 347
N Y Y 432 N Y N 532 N N Y 269 N N N 552
;
run;
proc catmod;
weight count;
model seatbelt*accident*death = _response_ / noiter pred=freq;
loglin seatbelt|accident|death;
run; quit; /* 모형이 맞지 않음 */

proc catmod;
weight count;
model seatbelt*accident*death = _response_ / noiter pred=freq;
loglin seatbelt|accident accident|death seatbelt|death;
run; quit; /* 모형이 일단 맞음. 하위모형과 최종비교를 위해 더 비교해보자. */

proc catmod;
weight count;
model seatbelt*accident*death = _response_ / noiter pred=freq;
loglin  accident|death seatbelt|death;
run; quit; /*(YZ,XZ) */

proc catmod;
weight count;
model seatbelt*accident*death = _response_ / noiter pred=freq;
loglin  seatbelt|accident seatbelt|death ;
run; quit; /* (XY,XZ) */

proc catmod;
weight count;
model seatbelt*accident*death = _response_ / noiter pred=freq;
loglin seatbelt|accident accident|death ;
run; quit; /* (XY,YZ) */

/*****************************************************************/

proc catmod;
weight count;
model seatbelt*accident*death = _response_ / noiter pred=freq;
loglin  seatbelt|accident death;
run; quit; /*(XY,Z) */

proc catmod;
weight count;
model seatbelt*accident*death = _response_ / noiter pred=freq;
loglin  seatbelt accident|death ;
run; quit; /*(X, YZ) */

proc catmod;
weight count;
model seatbelt*accident*death = _response_ / noiter pred=freq;
loglin  seatbelt|death accident ;
run; quit; /*(XZ, Y) */

/*****************************************************************/

proc catmod;
weight count;
model seatbelt*accident*death = _response_ / noiter pred=freq;
loglin  seatbelt death accident ;
run; quit; /*(X,Y, Z) */

proc export data=fourseven
outfile = 'C:\Users\82104\Desktop\4_7.txt'
dbms = dlm replace;
run;
