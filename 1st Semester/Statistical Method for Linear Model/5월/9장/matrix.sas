/* This code is stored as

          matrix.sas

   It uses the IML procedure in SAS.  
   The start command gives a name to a 
   set (or module) of commands.  The 
   finish command specifies the end of 
   the module.  This module can be run 
   anywhere in your SAS code by applying 
   the run command to the name of the  
   module.*/

proc iml;

/* Nmae the module matrix1 */

  start matrix1;

/*  Add and subtract matices  */

a = {3 6 ,
     2 1};

b = {7 -4,
     -3 2};

c = a+b;

d = a-b;

print a, b, c, d;


/* Compute the sum of all elements
   in a matrix  */

suma=sum(a);
print a suma;

/* Compute row sums  */

rowsuma = a[ ,+];
print 'row sums' rowsuma;

/* Compute column sums  */

colsuma = a[+, ];
print 'columns sums' colsuma;

/* Select the first row of a */

row1 = a[1, ];
print 'row1' row1;

/* Select the second column of a */

col2 = a[ ,2];
print 'col2' col2;

/* Select the element in the first
   row and the second column of a */

a12 = a[1,2];
print a12;

/*  Multiplication by a scalar */

c = {2 -1 3, 0 4 -2 };

d = 2*c;

print c, d;


/* Transpose of a matrix  */

ct = t(c);

print c, ct;



/* Matrix multiplication   */

a = {3 0 -2, 1 -1 4};

b = {1 1, 1 2, 1 3};

c = a*b;

print,,, a, b, c;


/* Here are two ways to compute
   the inner product of two 
   vectors.  The second method 
   is more efficient   */

x = {1, 7, -6, 4};

y = {2, -2, 1, 5};

xy1 = t(x)*y;

xy2 = sum(x#y);

print,,,, x, y, xy1, xy2;

/*  Length of a vector  */

ynorm = sqrt(sum(y#y));
print ynorm;

/*  Number of rows in the 
    column vector vector y */

ny = nrow(y);
print y ny;

/*  Elementwise multiplication */

  a = { 3 6, 2 1};
  b = { 7 -4, -3 2};
  ab = a#b;
  print,,, a, b, ab;

/* Kronecker Product */

  a = {2 4, 0 -2, 3 -1};
  b = {5 3, 2 1};
  ab = a@b; 
  print,,, a, b, ab;


/*  Create an identity matrix  */

  a = i(4);
  print a;

  a = i(3);
  print a;

/* Create a matrix where every
   element is the same.  The first
   argument is the number of rows,
   the second argument is the number
   of columns, the last argument is
   the value of each element */

  a = j(2,3,4.2);
  print a;


/*  Two methods of computing the
    trace of a matrix  */

  w = { 1 2 3, 4 5 6, 7 8 10};
  trw1 = trace(w);
  trw2 = sum(diag(w));
  print,,, w trw1 trw2;


/*  Inverse of a matrix  */

  winv = inv(w);
  prod = w*winv;
  print,,, w, winv, prod;


/* Determinant of a matrix */

  detw = det(w);
  print,,, w detw;

  x1 = { 1 2 3, 4 5 6, 7 8 9};
  detx1 = det(x1);
  print,,, x1 detx1;


/* IML does not have a function that
   computes the rank of a matrix. You
   can use the following which makes 
   use of the generalized inverse 
   function  */

  a = {1 1 1, 2 5 -1, 0 1 -1};
  ranka=round(trace(ginv(a)*a));
  print a ranka;

/*  You can also compute the QR 
    decomposition of the matrix.
    Here a = q*r    and 
    the number of linearly dependent
    columns of a is stored in lindep */

  CALL QR( q, r, piv, lindep, a);
  ranka = ncol(a)-lindep;
  print a, ranka, q, r; 


/*  Other examples  */

  c = {1 1 1, 2 5 -1, 0 1 1};
  rankc=round(trace(ginv(c)*c));
  print c rankc; 

  B = {1 1 0 0, 1 1 0 0,
       1 0 1 0, 1 0 1 0,
	   1 0 0 1, 1 0 0 1};
  rankB=round(trace(ginv(B)*B));
  print B rankB; 


/*  Note the rank function in IML
    is related to sorting.  It 
	computes the ranks of numbers
	in a matrix  */

 c = {1.2 5.1, 3.5 9.8};
 rc = rank(c);
 print,,,,, c, rc;



/* Eigenvalues and eigenvectors: 
    In call eigen(e, u, a) the 
    eigenvalues of a are stored in 
    the vector e and the eigenvectors 
    of a are stored in u.             */


 a = {1.96 0.72, 0.72 1.54};
 
 call eigen( e, u, a);
 print,,, a,,, 'Eigenvalues and eigenvectors', e u;



/* Singular value decomposition:
   The singular values are stored in 
   q in the following example and
       a = u*diag(q)*t(v) */

  a = { 2 0 1 1, 0 2 1 1, 1 1 1 1};

  call svd(u, q, v, a);
  ar = u*diag(q)*t(v);
  print a, 'Singular value decomposition',
        u, q, v, ar;


/*  An example where the singular
    values are the eigenvalues  */

 a = {1.96 0.72, 0.72 1.54};
 call svd( u, q, v, a);
 print a, 'Singular value decomposition', 
       u q v;  


/* Eigenvalues of a square symmetric 
   positive definite matrix are 
   all positive.  They match the 
   singular values.  */

 a = {4 2 -1, 2 6 -4, -1 -4 9};
 call eigen( e, u, a);
 print,,, a,,, 'Eigenvalues and eigenvectors', 
      e, u;
 call svd( u, q, v, a);
 print 'Singular value decomposition', 
       u, q, v;  


/* Eigenvalues of a square symmetric 
   matrix that is not positive  
   definite.  How do they match the 
   singular values?  */

 a = {4 2 -1, 2 6 -4, -1 -4 -9};
 call eigen( e, u, a);
 print,,, a,,, 'Eigenvalues and eigenvectors',
       e, u;
 call svd( u, q, v, a);
 print 'Singular value decomposition', 
       u, q, v;  

 
/* Inverse of a non-singular matrix */

 a = {1.96 0.72, 0.72 1.54};
 ainv = inv(a);
 ident = a*ainv;
 print,, a, ainv, ident;


/* Use  the spectral decomposition
   to compute an inverse square root
   matrix */

 a = {1.96 0.72, 0.72 1.54};
 call eigen( e, u, a);
 print,,, a,,, 'Eigenvalues and eigenvectors', 
    e, u;
 ainvm2 = u*inv(diag(sqrt(e)))*t(u);
 ainv = ainvm2*ainvm2;
 ident = ainvm2*a*ainvm2;
 print ainvm2, ainv, ident;


 /*  Solve a set of linear equations
            Ab=x                    
     These two methods provide the
     same results here, but the solve
     function is more efficient and
     more accurate for larger sets of
     equations  */

  A = {1.96 0.72, 0.72 1.54};
  x = {1, 1};
  b1 = inv(A)*x;
  b2 = solve(A,x);

  print,, A, x, 'Solutions', b1, b2;

finish;


/*  Execute the set of commands in
    the module named matrix1. The
    results requested by the print
    commands appear in the output
    window   */

run matrix1;
