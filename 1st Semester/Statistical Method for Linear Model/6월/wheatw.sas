
/*  Analyze the wheat data on assignment 6 */

data wheat;
   infile "c:\stat504\wheatw.dat";
   input tray moist fert Y;
   mpf = moist+fert;
   m = moist-25;
   f = fert - 5;
   m2 = m*m;
   mf = m*f;
   f2 = f*f;

 run;



/*  Print the data  */

proc print data=wheat;
run;

proc sort data=wheat; 
   by moist fert;
run;

proc means data=wheat noprint; 
   by moist fert; 
   var y;
   output out=avg mean=y;
run;
               
 

/* Compute sample means for all factor combinations
   with data.  Make a profile plot. */


proc sort data=wheat; by moist fert;
proc means data=wheat noprint; by moist fert;
  var Y;
  output out=means mean=my;
  run;

       SYMBOL1 V=circle  i=join ;
       SYMBOL2 V=diamond i=join ;
       SYMBOL3 V=square  i=join ;

 proc gplot data=means;
   plot my*moist=fert ;
   title  F=swiss "Fertilizer Profiles";
       label my='Means ';
       label moist = 'Moisture (ml/pot/day) ';
 run;

proc gplot data=means;
   plot my*fert=moist ;
   title  F=swiss "Moisture Profiles";
       label my=' ';
       label fert = 'Fertilizer (mg)';
 run;

  proc glm data=wheat;
    class moist fert tray;
    model y = moist tray(moist) fert moist*fert / solution
               ss1 ss3 e1 e3 p;
    random tray(moist) / q test;
	test h=moist e=tray(moist) / htype=3;
	lsmeans moist / pdiff tdiff stderr e=tray(moist);
	lsmeans fert / pdiff tdiff stderr;
    lsmeans moist*fert / pdiff tdiff stderr;
    run;

proc mixed data=wheat;
  class tray moist fert;
  model y = moist fert moist*fert / solution 
             ddfm=satterth e1 e3 outpm=set1;
  random tray(moist)/ solution;
  ods output solutionr=setr;
  ods output covparms=setv;
    estimate 'moist1 - moist2' moist 1 -1 0 0 
        moist*fert .25 .25 .25 .25 -.25 -.25 -.25 -.25
		           0 0 0 0 0 0 0 0;
    lsmeans moist / pdiff tdiff adjust=Tukey;;
	lsmeans fert / pdiff tdiff adjust=Tukey;;
    lsmeans moist*fert / pdiff adjust=Tukey; 
  run;

proc print data=setr; run;

proc print data=setv; run;


proc rank data=setr normal=blom out=setr ; 
  var estimate; ranks q;  run;

 SYMBOL1 V=circle  i=none ;
      

 proc gplot data=setr;
   plot estimate*q ;
   title  F=swiss "Normal Plot: Tray Effects";
       label q='Standard Normal Quantiles ';
       label clinic = 'Effects ';
 run;

proc rank data=set1 normal=blom out=set1 ; 
  var resid; ranks q;  run;

SYMBOL1 V=circle i=none ;
      

 proc gplot data=set1;
   plot resid*q ;
   title  F=swiss "Normal Plot: Residuals";
       label q='Standard Normal Quantiles ';
       label resid = 'Residuals';
 run;

 
 proc gplot data=set1;
   plot resid*pred;
   title  F=swiss "Residual Plot";
       label pred='Estimated Means';
       label resid = 'Residuals';
 run;




proc mixed data=wheat;
   class tray fert moist;
   model y = m f mf m2 f2 / solution e1 e3
              ddfm=satterth  ;
   random tray / solution;
   estimate "mean at fert=5 moist=15" intercept 1
       m -10 f 0 mf 0 m2 100 f2 0 / alpha=.05 CL;
run;

quit;
