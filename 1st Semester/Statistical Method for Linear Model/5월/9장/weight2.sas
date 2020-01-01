/*  SAS code for analzing repeated measures data across time
    (longitudinal studies).  This code is applied to the
    weightliting data from Littel, et. al. 1991)  This code
    is stored in the file.

                     weight2.sas     */


data set1;
   infile 'c:\stat504\weight2.dat';
   input subj program $ s1 s2 s3 s4 s5 s6 s7;
   if program='XCONT' then cprogram=3;
   if program='RI' then cprogram=1;
   if program='WI' then cprogram=2;
   run;;


/* Create a data file where responses at different time
    points are on different lines  */

data set2;
   set set1;
   time=2; strength=s1; output;
   time=4; strength=s2; output;
   time=6; strength=s3; output;
   time=8; strength=s4; output;
   time=10; strength=s5; output;
   time=12; strength=s6; output;
   time=14; strength=s7; output;
   keep subj program cprogram time strength;
run;

proc sort data=set2;
   by cprogram time;
run;


/* Create a profile plot with time on the horizontal axis */

proc means data=set2 noprint;
   by cprogram time;
   var strength;
   output out=seta mean=strength;
run;


   axis1 label=(f=swiss h=2.5)
      value=(f=swiss h=2.0)  w=3.0  ;

   axis2 label=(f=swiss h=2.2 a=270 r=90)
      value=(f=swiss h=2.0) w= 3.0  ;

   SYMBOL1 V=circle H=2.0 w=3 l=1 i=join ;
   SYMBOL2 V=diamond H=2.0 w=3 l=3 i=join ;
   SYMBOL3 V=square H=2.0 w=3 l=9 i=join ;

 proc gplot data=seta;
   plot strength*time=cprogram / vaxis=axis2 haxis=axis1;
   title  H=3.0 F=swiss "Observed Strength Means";
       label strength=' ';
       label time = 'Time (days) ';
 run;


/*  Fit the standard mixed model with a
    compound symmetric covariance structure
     by specify a random subject effect  */

proc mixed data=set2;
   class program subj time;
   model strength = program time program*time / dfm=satterth;
   random subj(program) / cl;
   lsmeans program / pdiff tdiff;
   lsmeans time / pdiff tdiff;
   lsmeans program*time / slice=time pdiff tdiff;
run;


/* Use the GLM procedure in SAS to get formulas for
   expectations of mean squares.  */


proc glm data=set2;
   class program subj time;
   model strength = program subj(program) time program*time;
   random subj(program);
   lsmeans program / pdiff tdiff;
   lsmeans time / pdiff tdiff;
   lsmeans program*time / slice=time pdiff tdiff;
run;


/* Fit the same model with the repeated statement.
   Here the compound symmetry covariance structure
   is selected.                                 */

proc mixed data=set2;
   class program subj time;
   model strength = program time program*time;
   repeated / type=cs sub=subj(program) r rcorr;
run;


/* Fit a model with the same fixed effects,
   but change the covariance structure to
   an AR(1) model                         */

proc mixed data=set2;
   class program subj time;
   model strength = program time program*time;
   repeated / type=ar(1) sub=subj(program) r rcorr;
run;


/* Fit a model with the same fixed effects,
   but use an arbitary covariance matrix
   for repated measures on the same subject. */

proc mixed data=set2;
   class program subj time;
   model strength = program time program*time;
   repeated / type=un sub=subj(program) r rcorr;
run;


/*  Fit a model with linear and quadratic
    trends across time and different trends
    across time for different programs.   */

proc mixed data=set2;
   class program subj;
   model strength = program time time*program
   time*time time*time*program / htype=1;
   repeated / type=ar(1) sub=subj(program);
run;



/*  Fit a model with linear, quadratic and
    cubic trends across time and different
    trends across time for different programs. */

proc mixed data=set2;
   class program subj;
   model strength = program time time*program
   time*time time*time*program
   time*time*time time*time*time*program / htype=1;
   repeated / type=ar(1) sub=subj(program);
run;


/* By removing the automatic intercept and adding
   the solution option to the model statement, the
   estimates of the parameters in the model are obtained.
   Here we use a power function model for the covariance
   structure.  This is a generalization of the AR(1)
   covariance structure that can be used with unequally
   spaced time points. */

proc mixed data=set2;
   class program subj;
   model strength = program time*program time*time*program
                        / noint solution htype=1;
   repeated / type=sp(pow)(time) sub=subj(program) r rcorr;
run;


/*  Fit a model with random coefficients that allow
    individual subjects to have different linear and
    quadratic trends across time.  */


proc mixed data=set2 scoring=8;
   class program subj;
   model strength = program time*program time*time*program/
                          noint solution htype=1;
   random int time time*time / type=un sub=subj(program)
                               solution ;
run;
