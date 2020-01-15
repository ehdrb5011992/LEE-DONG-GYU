DATA fourseven;
INPUT Seatbelt$ Accident$ Death$ count @@;
CARDS ;
Y Y Y 659 Y Y N 270 Y N Y 532 Y N N 347
N Y Y 432 N Y N 532 N N Y 269 N N N 552
;
run;

proc logistic desc;
class seabelt accident;
freq count;
model death = seatbelt accident / scale = none aggregate;
run; quit;
