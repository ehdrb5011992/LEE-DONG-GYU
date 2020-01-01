options ps=80 nocenter;

/*  SAS code for analyzing the data from the
    split plot experiment corresponding to
    example 9.3 in class   */


data set1;
  infile 'c:\stat504\grass.dat';
  input block cultivar $ innoc $ yield;
  run;


proc print data=set1;  run;


proc mixed data=set1;
  class block cultivar innoc;
  model yield = cultivar innoc cultivar*innoc / ddfm=satterth;
  random block block*cultivar;
  lsmeans cultivar / e  pdiff tdiff;
  lsmeans innoc / e pdiff tdiff;
  lsmeans cultivar*innoc / e pdiff tdiff;
  estimate  'a:live vs b:live'  cultivar 1 -1 innoc 0 0 0
                                cultivar*innoc 0 0 1 0 0 -1;
  estimate  'a:live vs b:live'  cultivar 1 -1 innoc 0 0 1
                          cultivar*innoc 0 0 1 0 0 -1 / e;
  estimate  'a:live vs a:dead'  cultivar 0 0 innoc 0 -1 1
                          cultivar*innoc 0 -1 1 0 0 0 / e;
run;



/*  Use Proc GLM to compute an ANOVA table */

proc glm data=set1;
  class block cultivar innoc;
  model yield = cultivar block cultivar*block
                innoc cultivar*innoc;
  random block block*cultivar;
  test h=cultivar block e=cultivar*block;
run;

quit;
