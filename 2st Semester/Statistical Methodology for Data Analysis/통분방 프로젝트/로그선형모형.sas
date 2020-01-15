data loglinear ;
infile 'C:\Users\82104\Desktop\loglinear.csv' dsd firstobs=2 ;
INPUT No pe$ group$  aa1$ ;
run;

/*  X : pe Y : group Z : aa1  */

/*포화모형 X*Y*Z */
proc catmod order=data data=loglinear ;
model pe * group * aa1 = _response_ / noiter pred=freq;
loglin pe|group|aa1 ;
run; quit;

/*하위모형 1 XY,YZ,XZ */
proc catmod order=data data=loglinear ;
model pe * group * aa1 = _response_ / noiter pred=freq;
loglin pe|group group|aa1 pe|aa1 ;
run; quit;

/*하위모형 2-1 XY,YZ   */
proc catmod order=data data=loglinear ;
model pe * group * aa1 = _response_ / noiter pred=freq;
loglin pe|group group|aa1;
run; quit;

/*하위모형 2-2  XY,XZ */
proc catmod order=data data=loglinear ;
model pe * group * aa1 = _response_ / noiter pred=freq;
loglin pe|group  pe|aa1 ;
run; quit;

/*하위모형 2-3 YZ,XZ */
proc catmod order=data data=loglinear ;
model pe * group * aa1 = _response_ / noiter pred=freq;
loglin  group|aa1 pe|aa1 ;
run; quit;


/*하위모형 3-1 XY,Z <---  이게 최종모형이다. */ 
proc catmod order=data data=loglinear ;
model pe * group * aa1 = _response_ / noiter pred=freq;
loglin  pe|group aa1 ;
run; quit;


/*하위모형 3-2 X,YZ */
proc catmod order=data data=loglinear ;
model pe * group * aa1 = _response_ / noiter pred=freq;
loglin  pe group|aa1 ;
run; quit;

/*하위모형 3-3 XZ,Y */
proc catmod order=data data=loglinear ;
model pe * group * aa1 = _response_ / noiter pred=freq;
loglin  pe|aa1 group ;
run; quit;

/*하위모형 4 X,Y,Z */
proc catmod order=data data=loglinear ;
model pe * group * aa1 = _response_ / noiter pred=freq;
loglin  pe aa1 group ;
run; quit;
