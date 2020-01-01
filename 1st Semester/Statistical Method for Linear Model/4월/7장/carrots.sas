/*  SAS code for analyzing the carrot germination data
    presented in the leture notes  */

data set1;
  input soil variety y;
  cards;
 1  1   6
 1  1  10
 1  1  11
 1  2  13
 1  2  15
 1  3  14
 1  3  22
 2  1  12
 2  1  15
 2  1  19
 2  1  18
 2  2  31
 2  3  18
 2  3   9
 2  3  12
run;


/*  Print the data  */

proc print data=set1;
run;



/* Compute sample means of germination times for all combinations
   of soil type and varieties of carrot seeds.  Make a profile plot. */


proc sort data=set1; by soil variety;
proc means data=set1 noprint; by soil variety;
  var Y;
  output out=means mean=my;
  run;

axis1 label=(f=swiss h=2.5)
      value=(f=swiss h=2.0)  w=3.0  length= 6.0 in;

axis2 label=(f=swiss h=2.2)
      value=(f=swiss h=2.0) w= 3.0  length = 4.5 in;

       SYMBOL1 V=circle H=2.0 w=3 l=1 i=join ;
       SYMBOL2 V=diamond H=2.0 w=3 l=3 i=join ;
       SYMBOL3 V=square H=2.0 w=3 l=9 i=join ;

 proc gplot data=means;
   plot my*variety=soil / vaxis=axis2 haxis=axis1;
   title  H=3.0 F=swiss "Average Time to Carrot Seed Germination";
       label my='Mean Time';
       label variety = 'Variety';
 run;


/*  Perform analysis of variance where soils are entered into
    the model before varieties.  Use the LSMEANS statement
    to compare soil types within varieties and to compare
    variety means within soil types.  */

  proc glm data=set1;
    class soil variety;
    model y = soil variety soil*variety / solution
               ss1 ss2 ss3 ss4 e e1 e2 e3 e4 p;
    means soil variety soil*variety;
    lsmeans soil*variety / pdiff tdiff stderr;
    estimate 'soil 1 vs soil 2' soil 1 -1 / e;
    contrast 'soil 1 vs soil 2' soil 1 -1 / e;
    estimate 'soil 1 vs 2 averaged' soil 3 -3
              soil*variety 1 1 1 -1 -1 -1 / divisor=3 e;
    estimate 'soil 1 vs 2 for var1'  soil 1 -1
              soil*variety 1 0 0 -1 0 0 / e;
    estimate 'soil 1 vs 2 for var2'  soil 1 -1
              soil*variety 0 1 0 0 -1 0 / e;
    estimate 'soil 1 vs 2 for var3'  soil 1 -1
              soil*variety 0 0 1 0 0 -1 / e;
    run;

proc iml;

start new;

  x = { 1 1 1 0 1 0,
        1 1 1 0 1 0,
        1 1 1 0 1 0,
        1 1 0 1 0 1,
        1 1 0 1 0 1,
        1 1 -1 -1 -1 -1,
        1 1 -1 -1 -1 -1,
        1 -1 1 0 -1 0,
        1 -1 1 0 -1 0,
        1 -1 1 0 -1 0,
        1 -1 1 0 -1 0,
        1 -1 0 1 0 -1,
        1 -1 -1 -1 1 1,
        1 -1 -1 -1 1 1,
        1 -1 -1 -1 1 1};

  y = {6 10 11 13 15 14 22 12 15 19 18 31 18 9 12};
  y = t(y);

  b = inv(t(x)*x)*t(x)*y;

  print b;

  z = x||y;

  create set1 from z;
  append from z;


  /*  Compute type III sums of squares and F-tests  */

  D = {1 1 1 0 0 0 0 0 0 0 0 0 0 0 0,
       0 0 0 1 1 0 0 0 0 0 0 0 0 0 0,
       0 0 0 0 0 1 1 0 0 0 0 0 0 0 0,
       0 0 0 0 0 0 0 1 1 1 1 0 0 0 0,
       0 0 0 0 0 0 0 0 0 0 0 1 0 0 0,
       0 0 0 0 0 0 0 0 0 0 0 0 1 1 1};

  d = t(d);
  b = inv(t(d)*d)*t(d)*y;
  s = 2;
  t = 3;
  c1 = (i(s-1)||-j(s-1,1))@j(1,t);
  q1 = t(b)*t(c1)*inv(c1*inv(t(d)*d)*t(c1))*c1*b;
  yhat=d*b;
  sse = ssq(y)-ssq(yhat);
  df1=s-1;
  df2=nrow(y)-s*t;
  f = (q1/df1)/(sse/df2);
  p1 = 1- probf(f,df1,df2);
  print c1 q1 f p1;

  c2 = j(1,s)@(i(t-1)||-j(t-1,1));
  q2 = t(b)*t(c2)*inv(c2*inv(t(d)*d)*t(c2))*c2*b;
  df1=t-1;
  df2=nrow(y)-s*t;
  f = (q2/df1)/(sse/df2);
  p = 1- probf(f,df1,df2);
  print c2 q2 f p;

  c3 = (i(s-1)||-j(s-1,1))@(i(t-1)||-j(t-1,1));
  q3 = t(b)*t(c3)*inv(c3*inv(t(d)*d)*t(c3))*c3*b;
  df1=(s-1)*(t-1);
  df2=nrow(y)-s*t;
  f = (q3/df1)/(sse/df2);
  p = 1- probf(f,df1,df2);
  print c3 q3 f p;


  finish;

  run new;

  data set1;  set set1;
  if(col2 = 1) then soil=1; else soil=2;
  if(col3 = 1) then variety =1; else variety=3;
  if(col4 = 1) then variety = 2;
  y = col7;
  run;
