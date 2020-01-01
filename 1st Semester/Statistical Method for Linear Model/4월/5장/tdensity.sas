
/* program to generate plots of density
   functions for central t-distributions */

proc iml;

    start tden;
    z = j(1000,5);

    do  i= 1 to 1000;
    x = (i-500)/100;

    v=2;              /*  Set the degrees of freedom  */

                     /*  evaluate the central t density  */

    f2 = ((1+(x*x/v))**(-(v+1)/2))/(sqrt(v)*gamma(.5)*gamma(v/2)/gamma(.5*v+.5));

     v=5;              /*  Set the degrees of freedom  */

                     /*  evaluate the central t density  */

    f5 = ((1+(x*x/v))**(-(v+1)/2))/(sqrt(v)*gamma(.5)*gamma(v/2)/gamma(.5*v+.5));


     v=10;              /*  Set the degrees of freedom  */

                     /*  evaluate the central t density  */

    f10 = ((1+(x*x/v))**(-(v+1)/2))/(sqrt(v)*gamma(.5)*gamma(v/2)/gamma(.5*v+.5));


                  /* Evaluate the standard normal density  */

    fn = (exp(-x*x/2))/sqrt(2*3.14159265359);

    z[i,] = x||f2||f5||f10||fn;
    end;

    create set1 from z;
       append from z;


    finish;


  run tden;


  axis1 length = 5 in
        color = black width=8.0 style=1
        order = 0 to 0.4 by .1;

  axis2 label = (h=2.5 f=swiss 't')
        length = 5.5 in
        color=black width=8.0 style=1
        order = -4 to 4 by 1;

  symbol1 v=none i=spline l=1 h=2 c=black width=8;
  symbol2 v=none i=spline l=3 h=2 c=black width=8;
  symbol3 v=none i=spline l=9 h=2 c=black width=8;
  symbol4 v=none i=spline l=16 h=2 c=black width=8;

proc gplot data=set1;
  plot (col5 col2 col3 col4)*col1 / overlay vaxis=axis1 haxis=axis2;
  title h=3 f=swiss  'Student t Densities';
  run;

proc gplot data=set1;
  plot (col3)*col1 / vaxis=axis1 haxis=axis2;
  title h=3 f=swiss  'Student t Density';
  title2 h=2.5 f=swiss '(5 degrees of freedom)';
  run;
