options nocenter ps=80;
title;

/* This is a SAS program for analyzing
   data from a nested or heirarchical 
   experiment.  This program is posted as

            pigment.sas

   The data are measurements of moisture 
   content of a pigment taken from Box, 
   Hunter and Hunter (page 574).  */


data set1;
  infile 'c:\stat504\pigment.dat';
  input batch sample test y;
  run;


proc print data=set1;
  run;


/*  The  "random"  statement in the 
    following GLM procedure prints 
    formulas for expectations of 
    mean squares.    */

proc glm data=set1;
  class batch sample;
  model y = batch sample(batch) / e1;
  random batch sample(batch) / q test;
  run;


/*  Alternatively, REML estimates of variance
    components are  produced by the MIXED
    procedure in SAS.  Note that there are
    no terms on the rigth of the equal sign in
    the model statement because the only
    non-random effect is the intercept.        */


proc mixed data=set1;
  class batch sample test;
  model y = ;
  random batch sample(batch);
  run;



/*  Use the MIXED procedure in SAS to compute
    maximum likelihood estimates of variance
    components */


proc mixed data=set1 method=ml;
  class batch sample test;
  model y = ;
  random batch sample(batch);
  run;
