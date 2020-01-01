/*  SAS code for analzing the ice crsytal
    growth data stored at EKU             */


data set1;
   infile 'c:\stat504\crystals.dat';
   input time length;
   run;

/* Fit a straight line model  */

proc reg data=set1;
  model length = time / ss1 ss2;
  output out=set2 residual=resid predicted=yhat;
  run;

/* Plot the observations and the estimated line  */

symbol1 interpol=none
        value=dot
        color=_style_;
symbol2 interpol=spline
        value=C
        font=marker
        color=_style_ ;
  
 proc gplot data=set2;
   plot (length yhat)*time / overlay ;
   title  "Ice Crystal Growth";
       label length='Axial Length ';
       label time = 'Time(seconds) ';
 run;


/* Construct a normal probability plot
   and other residual plots  */
   
proc rank data=set2 normal=blom out=set2;
  var resid;
  ranks q;
  run;

       SYMBOL1 V=circle H=1.0 w=2 l=1 i=none ;
 
 proc gplot data=set2;
   plot resid*time ;
   title   "Residuals vs. Time";
       label resid='Residuals ';
       label time = 'Time(seconds) ';
 run;
 
 proc gplot data=set2;
   plot resid*q ;
   title  "Normal Probability Plot";
       label resid='Residuals ';
       label q = 'Standard Normal Quantiles ';
 run;


/* Fit a quadratic model  */

data set1; set set1;
  time2=time*time;
  run;

proc reg data=set1;
  model length = time time2/ ss1 ss2;
  output out=set3 residual=resid predicted=yhat;
  run;

/* Plot the observations and the estimated line  */


symbol1 interpol=none
        value=dot
        color=_style_;
symbol2 interpol=spline
        value=C
        font=marker
        color=_style_ ;
    

 proc gplot data=set3;
   plot (length yhat)*time / overlay ;
   title  "Ice Crystal Growth";
       label length='Axial Length ';
       label time = 'Time(seconds) ';
 run;


/* Construct a normal probability plot
   and other residual plots  */
   
proc rank data=set3 normal=blom out=set3;
  var resid;
  ranks q;
  run;

       SYMBOL1 V=circle H=2.0 w=3 l=1 i=none ;
 
 proc gplot data=set3;
   plot resid*time;
   title   "Residuals vs. Time";
       label resid='Residuals ';
       label time = 'Time(seconds) ';
 run;
 
 proc gplot data=set3;
   plot resid*q;
   title   "Normal Probability Plot";
       label resid='Residuals ';
       label q = 'Standard Normal Quantiles ';
 run;


data set1; set set1;
   time2 = time*time;
   run;

proc reg data=set1;
  model length = time time2/ ss1 ss2;
  output out=set2 residual=resid predicted=yhat;
  run;


/* Peform a goodness of fit test 
   for the quadratic model  */


data set1; set set1;
   group=time;
   run;

proc glm data=set1;
  class group;
  model length = time time2 group/ ss1 ss2;
  run;

  quit;
