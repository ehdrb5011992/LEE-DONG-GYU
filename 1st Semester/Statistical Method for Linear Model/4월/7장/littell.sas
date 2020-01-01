
/* SAS code for analyzing data
   from the two factor experiment
   with no data for one combination
   of factors>  This code is posted
   as  littell.sas   */


data set1;
  input A B y;
  cards;
 1  1   5
 1  1   6
 1  2   2
 1  2   3
 1  2   5
 1  2   6
 1  2   7
 2  1   2
 2  1   3
 2  2   8
 2  2   8
 2  2   9
 2  3   4
 2  3   4
 2  3   6
 2  3   6
 2  3   7
run;


/*  Print the data  */

proc print data=set1;
run;


/* Compute sample means for all
   factor combinations with data.
   Make a profile plot. */

proc sort data=set1; by a b;
proc means data=set1 noprint; by a b;
  var Y;
  output out=means mean=my;
  run;

axis1 label=(f=swiss h=2.0)
      value=(f=swiss h=1.8)
      w=3.0;

axis2 label=(f=swiss h=2.0 a=90 r=0)
      value=(f=swiss h=1.8)
      w= 3.0;

SYMBOL1 V=circle H=2.0 w=3 l=1 i=join;
SYMBOL2 V=diamond H=2.0 w=3 l=3 i=join;

proc gplot data=means;
  plot my*b=a / vaxis=axis2 haxis=axis1;
  title ls=0.8in H=3.0 F=swiss "Sample Means";
       label my='Mean';
       label b = 'Factor B';
  footnote ls=0.4in '  ';
 run;


/* Perform analysis of variance where
   facror A is entered into the model
   before factor B.  Use the LSMEANS
   statement to compare means for
   different combinations of factor A
   and factor B.  */

proc glm data=set1;
  class A B;
  model y = A B A*B / solution ss1 ss2
            ss3 ss4 e e1 e2 e3 e4 p;
  means A B A*B;
  lsmeans A*B / pdiff tdiff stderr;
    estimate 'A1-A2' A 1 -1 / e;
    contrast 'A1-A2' A 1 -1 / e;
    estimate 'A1-A2 within B1'  A 1 -1
              A*B 1 0  -1 0 0 / e;
    estimate 'A1-A2 within B2'  A 1 -1
              A*B 0 1 0 -1 0 / e;
    estimate 'A1-A2 over B'  A 1 -1
              A*B .5 .5 -.5 -.5 0 / e;
    estimate 'B1-B2 over A'  B 1 -1 0
              A*B .5 -.5 .5 -.5 0 / e;
    estimate 'B3-.5(B1+B2) in A2' B -.5 -.5 1
              A*B 0 0 -.5 -.5 1 / e;
    estimate 'interaction' A*B 1 -1 -1 1 0 / e;
    run;


/*  Do everything with a one-factor ANOVA by
    combining the two factors into a single
    factor with 5 categories.  */


data set1; set set1;
  C=10*A+B;
  run;

proc glm data=set1;
  class C;
  model y = C / solution e e2;
  estimate 'C11-C21' C 1 0 -1 0 0;
  estimate 'C12-C22' C 0 1 0 -1 0;
  estimate '.5(C11+C12-C21+C22)' C .5 .5 -.5 -.5 0;
  estimate '.5(C11-C12+C21-C22)' C .5 -.5 .5 -.5 0;
  estimate 'C23-.5(C21+C22)' C 0 0 -.5 -.5 1;
  estimate 'C11-C12-C21+C22' C 1 -1 -1 1 0;
  lsmeans C / stderr tdiff pdiff;
  run;
