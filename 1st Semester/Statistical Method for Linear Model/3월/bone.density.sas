/* Read the bone density data from an
   EXCEL spreadsheet.  */

PROC IMPORT OUT= WORK.SET1 
            DATAFILE= "C:\stat504\bone2.density.xls" 
            DBMS=EXCEL REPLACE;
     SHEET="bone3#density$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

/* Examine the contents of the file */

proc contents data=set1; run;


/* Fit a model  */

proc glm data=set1;
  class group;
  model bmd = age group / solution ss1 ss3;
  output out=set1 residual=r predicted=p;
  lsmeans group /stderr pdiff;
run;

/* Compute plotting positions for a normal probability plot  */

proc rank data=set1 normal=blom out=set1;
  var r; ranks q;
run;

/* Residual Plots */	

axis1 label=(f=swiss "Predicted BMD (g/cm^2)")  
      value=(f=swiss); 

axis2 label=(f=swiss "Residuals")  
      value=(f=swiss); 

axis3 label=(f=swiss "q")
      order= -3 to 3 by 1 
      value=(f=swiss); 

 SYMBOL1 V=CIRCLE l=1 i=none ;
 SYMBOL2 V=DIAMOND  l=3 i=join ;
 SYMBOL3 V=square  l=9 i=join ;
 

 PROC GPLOT DATA=set1; 
   PLOT r*p / vaxis=axis2 haxis=axis1;
   TITLE1 ls=0.01in H=2.0 F=swiss "Residuals vs. Prdeicted Values";
	   footnote ls=0.01in;
RUN;

 PROC GPLOT DATA=set1; 
   PLOT r*q / vaxis=axis2 haxis=axis3;
   TITLE1 ls=0.01in H=2.0 F=swiss "Normal Probability Plot";
	   footnote ls=0.01in;
RUN;

quit;
