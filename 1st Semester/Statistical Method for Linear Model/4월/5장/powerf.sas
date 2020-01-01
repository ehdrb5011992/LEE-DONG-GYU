
/* Program for plotting the power curve of an F-test
   of the null hypothesis of equal treatment means
   against the sample size for a one-way ANOVA.
   Here  n  represents the number of observations
   for each treatment.                             */


proc iml;

    start powerf;

    alpha = .05;         /* Enter the Type I error level  */

    k = 3;               /* Enter the number of treatments  */

    n1 = 2;              /* Enter the lower bound on the sample size */
    n2 = 50;             /* Enter the upper bound on the sample size */
    ninc = 2;            /* Enter the sample size increment */

    nstep = int((n2-n1)/ninc) + 1;
    power = j(nstep,1,0);
    nobs = j(nstep,1,0);

    do  i= 1 to nstep;
      n = n1 + ninc*(i-1);   /*  compute the sample size */
      a=k-1; b=k*(n-1);      /*  Set the degrees of freedom  */

      d=n/2;                 /*  Evaluate the noncentrality parameter */
                        
                             /*  Evaluate power  */
      f = finv(1-alpha,a,b);
      power[i,1] =  1 - probf(f,a,b,d);
      nobs[i,1] = n;
      end;

  power = power||nobs;
  create out1 from power;
  append from power;

  finish;

run powerf;


  axis1 label = (h=3 r=0 a=90 f=swiss 'Power')
        length = 5 in
        color = black width=8.0 style=1;

  axis2 label = (h=2.5 f=swiss 'Sample size')
        length = 5.5 in
        color=black width=8.0 style=1;

  symbol1 v=none i=spline l=1 h=2 c=black width=8;

proc gplot data=out1;
  plot col1*col2 / vaxis=axis1 haxis=axis2;
  title h=3 f=swiss  'Power versus Sample Size';
  title2 h=2 f=swiss  'F-test for equal means';
  run;


/* Program for computing the smallest sample size
   that produces a desired power value of an F-test
   of a null hypothesis of the form CB=0.  */
  
proc iml;

    start powerfun;

    alpha = .05;         /* Enter the Type I error level  */
	power = .90;         /* Enter desired power */

                         /* Enter C matrix for a null 
	                        hypothesis of the form CB=0 */

     C = { 0 1 0 -1,
	       0 0 1 -1};
						 /* Enter deviations as a multiple of
							the error standard deviation  */
	 d= { .5, 1};

	                     /* Enter the rows of the model matrix that
	                        be replicated as the sample size increases */

    W={1 1 0 0,
	   1 0 1 0,
	   1 0 0 1};
	   					 /* Compute the rank of W */
	PW = W*GINV(T(w)*W)*T(w);
    call svd(U,Q,V,W);
	rankw = sum(q);
     
 
    n1 = 2;              /* Enter the lower bound on initial sample size*/
    npower=n1;

	X = W;               /* Set up initial model matrix  */
    do i = 2 to n1;
	  X = X//W;
	  end;

    n2 = 100;            /* Enter the upper bound on the sample size */
    ninc = 1;            /* Enter the sample size increment */

    nstep = int((n2-n1)/ninc) + 1;
   

    do  i= 1 to nstep;
      n = n1 + ninc*(i);   /*  compute the sample size */

	  do j = 1 to ninc;      /*  Expand the model matrix */
	  X=X//W;
	  end;

      a=nrow(C);            /*  Set the degrees of freedom  */
      b=nrow(X)-rankw;

                             /*  Evaluate the noncentrality parameter */
	  nc=t(d)*inv(C*ginv(t(X)*X)*t(C))*d;

                             /*  Evaluate power  */

      f = finv(1-alpha,a,b);
      if(1 - probf(f,a,b,nc)< power) then npower=n;
      end;

  npower=npower+1;
  if(npower< n1+ninc*nstep+1) then do;
  print "sample size; " npower;
  stop;
  end;

  print "Equired sample size is larger than " npower; 

  finish;

run powerfun;


