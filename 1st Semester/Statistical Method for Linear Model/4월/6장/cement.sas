/*  A SAS program to perform a regression
    analysis of the effects of the
    composition of Portland cement on the
    amount of heat given off as the cement
    hardens.  */

data set1;
  input run x1 x2 x3 x4 y;
  label y = evolved heat (calories)
        x1 = tricalcium aluminate
        x2 = tricalcium silicate
        x3 = tetracalcium aluminate ferrate
        x4 = dicalcium silicate;  
  cards;
  1  7 26  6 60  78.5
  2  1 29 15 52  74.3
  3 11 56  8 20 104.3
  4 11 31  8 47  87.6
  5  7 52  6 33  95.9
  6 11 55  9 22 109.2
  7  3 71 17  6 102.7
  8  1 31 22 44  72.5
  9  2 54 18 22  93.1
 10 21 47  4 26 115.9
 11  1 40 23 34  83.8
 12 11 66  9 12 113.2
 13 10 68  8 12 109.4
run;

proc print data=set1 uniform split='*';
  var y x1 x2 x3 x4;
  label y = 'Evolved*heat*(calories)'
        x1 = 'Percent*tricalcium*aluminate'
        x2 = 'Percent*tricalcium*silicate'
        x3 = 'Percent*tetracalcium*aluminate*ferrate'
        x4 = 'Percent*dicalcium*silicate';
  run;

/*  Regress y on all four explanatory
    variables and check residual plots
    and collinearity diagnostics */

proc reg data=set1 corr;
  model y = x1 x2 x3 x4 / p r ss1 ss2
                          covb  collin;
  output out=set2 residual=r
                      predicted=yhat;
  run;


/*  Examine smaller regression models
    corresponding to subsets of the
    explanatory variables  */


proc reg data=set1;
  model  y = x1 x2 x3 x4 /
              selection=rsquare cp aic
              sbc mse stop=4 best=6;
  run;



/*  Regress y on two of explanatory
    variables and check residual plots
    and collinearity diagnostics */

proc reg data=set1 corr;
  model y = x1 x2 / p r ss1 ss2
                        covb  collin;
  output out=set2 residual=r
                      predicted=yhat;
  run;


/*  Use the GLM procedure to identify
    all estimable functions  */

proc glm data=set1;
  model y = x1 x2 x3 x4 / ss1 ss2 e1 e2 e p;
  run;

quit;
