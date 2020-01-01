options nocenter ps=80;
title;

/*  This is a program for analyzing the 
    penicillan data from Box, Hunter, 
    and Hunter.  It is posteded in the 
    file
              penclln.sas          */

/*  First enter the data  */


data set1;
  infile 'c:\stat504\penclln.dat';
  input batch process $ yield;
  run;


/* Compute the ANOVA table, formulas for
   expectations of mean squares, process
   means and their standard errors */

proc glm data=set1;
  class batch process;
  model yield = batch process / e e3;
  random batch / q test;
  lsmeans process / stderr pdiff tdiff;
  output out=set2 r=resid p=yhat;
  run;

/* Compute a normal probability plot for 
   the residuals and the Shapiro-Wilk 
   test for normality  */

proc rank data=set2 normal=blom out=set2;
  var resid;  ranks q;
  run;

proc univariate data=set2 normal plot;
  var resid;
  run;

  axis1 label=(h=2.5 r=0 a=90 f=swiss 'Residuals')
        value=(f=swiss h=2.0) w=3.0 ;

  axis2 label=(h=2.3 f=swiss 'Standard Normal Quantiles')
        value=(f=swiss h=2.0) w=3.0 ;

  axis3 label=(h=2.3 f=swiss 'Production Process')
        value=(f=swiss h=2.0) w=3.0 ;

  symbol1 v=circle i=none h=2 w=3 c=black;

proc gplot data=set2;
  plot resid*q / vaxis=axis1 haxis=axis2;
  title h=3.0 ls=1.0in f=swiss 
        c=black 'Normal Probability Plot';
  footnote ls=0.6in '  ';
  run;

proc gplot data=set2;
  plot resid*process / vaxis=axis1 haxis=axis3;
  title h=3.0 ls=1.0in f=swiss 
        c=black 'Residual Plot';
  footnote ls=0.6in '   ';
  run;

quit;

/* Fit the same model using PROC MIXED.  Compute
   REML estimates of variance components.  Note
   that PROC MIXED provides appropriate standard
   errors for process means.  When block effects
   are random. PROC GLM does not provide correct
   standard errors for process means  */

title;

proc mixed data=set1;
  class process batch;
  model yield = process / ddfm=satterth solution;
  random batch / type=vc G solution cl alpha=.05;
  lsmeans process / pdiff tdiff;
run;

