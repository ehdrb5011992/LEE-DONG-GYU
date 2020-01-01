data set1;
  infile 'C:\stat504\cane.dat ';
  input variety nitrogen yield;
run;

/* Print the data */

proc print data=set1;
   var yield;
run;

/* Compute an ANOVA table */

proc glm data=set1;
  class variety nitrogen;
  model yield = variety|nitrogen /
          p clm alpha=.05  ss1 ss2
          ss3 ss4 e e1 e2 e3 e4;
  output out=setr r=resid p=yhat;
  lsmeans variety*nitrogen / stderr pdiff;
  means variety nitrogen / tukey;
  contrast 'n-linear' nitrogen -1 0 1;
  contrast 'n-quad' nitrogen -1 2 -1;
  contrast 'v1-v2' variety 1 -1 0;
  contrast '(v1+v2)-v3' variety .5 .5 -1;
  contrast '(v1-v2)*(n-lin)' variety*nitrogen
                      -1 0 1 1 0 -1 0 0 0;
  contrast '(v1-v2)*(n-quad)' variety*nitrogen
                      -1 2 -1 1 -2 1 0 0 0;
  contrast '(.5(v1+v2)-v3)*(n-lin)'
                 variety*nitrogen
                -.5 0 .5 -.5 0 .5 1 0 -1;
  contrast '(.5(v1+v2)-v3)*(n-quad)'
                 variety*nitrogen
               -.5 1 -.5 -.5 1 -.5 1 -2 1;
  estimate 'n-linear' nitrogen -1 0 1;
  estimate 'n-quad' nitrogen -1 2 -1;
  estimate 'v1-v2' variety 1 -1 0;
  estimate '(v1+v2)-v3' variety .5 .5 -1;
  estimate '(v1-v2)*(n-lin)' variety*nitrogen
                      -1 0 1 1 0 -1 0 0 0;
  estimate '(v1-v2)*(n-quad)' variety*nitrogen
                      -1 2 -1 1 -2 1 0 0 0;
  estimate '(.5(v1+v2)-v3)*(n-lin)' variety*nitrogen
                      -.5 0 .5 -.5 0 .5 1 0 -1;
  estimate '(.5(v1+v2)-v3)*(n-quad)' variety*nitrogen
                      -.5 1 -.5 -.5 1 -.5 1 -2 1;
  run;

/* Make a profile plots for the interaction
   between varieties and nitrogen levels  */

proc sort data=set1; by variety nitrogen;
proc means data=set1 noprint;
  by variety nitrogen;
  var yield;
  output out=means mean=my;
run;

axis1 label=(f=swiss h=2.5)
      ORDER = 120 to 300 by 30
      value=(f=swiss h=2.0)  w=3.0;


axis2 label=(f=swiss h=2.0)
      order = 50 to 80 by 10
      value=(f=swiss h=2.0) w= 3.0;

 SYMBOL1 V=CIRCLE H=2.0 w=3 l=1 i=join ;
 SYMBOL2 V=DIAMOND H=2.0 w=3 l=3 i=join ;
 SYMBOL3 V=square H=2.0 w=3 l=9 i=join ;

 PROC GPLOT DATA=means;
   PLOT my*nitrogen=variety /
             vaxis=axis2 haxis=axis1;
   TITLE1  H=3.0 F=swiss "Sugar Cane Yields";
       LABEL my='Mean Yield';
       LABEL nitrogen = 'Nitrogen (lb/acre)';
 RUN;
