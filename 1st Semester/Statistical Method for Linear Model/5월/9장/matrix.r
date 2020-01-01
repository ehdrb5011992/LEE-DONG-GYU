
#  This code is stored in the file
#
#        matrix.r
#

#-------------------------------
# Add and subtract matrices
#-------------------------------

  a<-matrix(c(3, 6, 2, 1),2,2,byrow=T)
  a

  aa<-matrix(c(3, 6, 2, 1),2,2,byrow=F)
  aa

  b<-matrix(c(7, -4, -3, 2),2,2,byrow=T)
  b

  a+b

  a-b

#---------------------------------
#  Multiplication by a scalar
#---------------------------------

  c<-matrix(c(2, -1, 3, 0, 4, -2),2,3,byrow=T)
  c

  d<-2*c
  d

#-------------------------------
#  Transpose of a matrix
#-------------------------------

  ct <- t(c)
  ct

#-------------------------------
#   Matrix multiplication 
#-------------------------------
 
  a<- matrix(c(3, 0, -2, 1, -1, 4), 2,3,byrow=T)
  a

  b<- matrix(c(1,1,1,2,1,3), 3,2,byrow=T)
  b

  c<-a%*%b
  c

#----------------------
#  Inner product
#----------------------
 
  x<-c(1,7,-6,4)
  y<-c(2,-2,1,5)
  x
  y


  t(x)%*%y
  x%*%y
  crossprod(x,y)

#----------------------
#  Length of a vector
#----------------------

  ynorm<-sqrt(crossprod(y,y))
  ynorm

#----------------------------------
#   Number of elements in a vector
#----------------------------------

  length(y)

#--------------------------------
#  Elementwise multiplication
#--------------------------------
 
  a<-matrix(c(1,2,3,4),2,2,byrow=T)
  a

  b<-matrix(c(3,-1,0,5),2,2,byrow=T)
  b

  a*b

# -------------------------
#   Kronecker Product)
# -------------------------

    a <- matrix(c(2,4,0,-2,3,-1),ncol=2,byrow=T)
    a

    b <- matrix(c(5,3,2,1),2,2,byrow=T)
    b

    kronecker(a,b)
 
#---------------------------------------
#   What happens when the dimensions
#   of the matrices or vectors are
#   not appropriate for the operation
#---------------------------------------

  a<-matrix(c(1, 1, 1, 2), 2, 2, byrow=T)
  b<-matrix(c(3, 0, -2, 1, -1, 4), 2, 3, byrow=T)
  a
  b

  a+b

  b+a

  a%*%b

  b%*%a

  a*b





# This sciprt is also stored in
# 
#        matrix.r
 

#----------------------------
#   Inverse of a matrix
#----------------------------

 w<-matrix(c(1,2,3,4,5,6,7,8,10),3,3,byrow=T) 
 w

 winv<-solve(w)
 winv

 w%*%winv


# ------------------------------
#   Determinant of a matrix
# ------------------------------


# Build your own function

 determ<-function(M) Re(prod(eigen(M, only.values=T)$values))
 determ(w)

#  Another function 

 absdet <- function(M) abs(prod(diag(qr(M)$qr)))
 absdet(w)


#  Another example

 x1 <- matrix(c(1,2,3,4,5,6,7,8,9),ncol=3,byrow=T)
 x1
 
 determ(x1)
 absdet(x1)
 

  
#--------------------------------------------
#  Rank of a matrix:  use the "qr" function 
#--------------------------------------------

 A <- matrix(c(1, 1,  1,
                 2, 5, -1,
                 0, 1, -1),3,3,byrow=T)
 A
 qr(A)$rank

#   Another example

 A <- matrix(c(1,1, 1,
                 2,5,-1,
                 0,1, 1),3,3,byrow=T)
 A
 qr(A)$rank

#  Another example

 X <- matrix(c(1,1,0,0,
                1,1,0,0,
                1,0,1,0,
                1,0,1,0,
                1,0,0,1,
                1,0,0,1),ncol=4,byrow=T)
 X
 qr(X)$rank


# Note that the sum of squares 
# and crossproducts matrix has  
# the same rank as X

 XtX <- t(X)%*%X
 XtX
 
 qr(XtX)$rank


# This is a square symmetric matrix
# but the inverse does not exist

 solve(XtX)
 
#  Note that the function "rank" in R
#  is related to sorting.  It computes the 
#  ranks of the elements of a vector.

 rank(c(1.2, 5.1, 3.5, 9.8))


# ---------------------------------
#  Create an nxn identity matrix
# ---------------------------------

  diag(rep(1,4))

#---------------------------
#  Trace of a matrix
#---------------------------

 w<-matrix(c(1,2,3,4,5,6,7,8,10),3,3,byrow=T) 
 w

 tr<-sum(diag(w))
 tr

#----------------------------------
#  Compute row sums or column sums
#----------------------------------

 sum(w)

 apply(w,1,sum)

 apply(w,2,sum)

 apply(w,1,prod)

 apply(w,1,mean)

 apply(w,1,var)

# ----------------------------------
#   Eigenvalues & Eigenvectors
# ----------------------------------

 A <- matrix(c(1.96,.72,.72,1.54),2,2,byrow=T)

 A

 EA <- eigen(A)
 EA 
 ls(EA)

 eigen(A)$values 

# ---------------------------------
#   Singular Value Decomposition
# ---------------------------------

   A <- matrix(c(2,0,1,1,0,2,1,1,1,1,1,1),
               ncol=4,byrow=T)
   A

   svdA <- svd(A)
   ls(svdA)
   svdA

   svdA$u %*% t(svdA$u)
   t(svdA$v) %*% svdA$v
   svdA$u%*%diag(svdA$d)%*%t(svdA$v)
   diag(svdA$d) %*%  diag(svdA$d)

   eigen(A%*%t(A))$values
   eigen(t(A)%*%A)$values

#  An example where the singular values
#  are the eigenvalues 

 A <- matrix(c(1.96,.72,.72,1.54),2,2,byrow=T)
 A

 svdA <- svd(A)
 svdA

 
#----------------------------------------
#  Trace and determinant of a matrix
#----------------------------------------
 
  A <- matrix(c(1,1, 1,
                2,5,-1,
                0,1, 1),3,3,byrow=T)

  A
  traceA <- sum(diag(A))
  traceA

  eigenA <- eigen(A)
  eigenA

  traceA <- sum(eigenA$values)
  traceA
 
  Re(traceA)

  detA <- Re(prod(eigenA$values))
  detA


#  An example where the eigenvalues
#  are real numbers

  A <- matrix(c(1,1, 1,
                2,5,-1,
                0, 1, -1),3,3,byrow=T)

  A
     [,1] [,2] [,3] 
[1,]    1    1    1
[2,]    2    5   -1
[3,]    0    1   -1

 eigenA <- eigen(A)
 eigenA

 traceA <- sum(eigenA$values)
 traceA

 detA <- Re(prod(eigenA$values))
 detA



#-------------------------------------------
#  Eigenvalues of a square symmetric matrix
#-------------------------------------------

 A<-matrix(c(4,2,-1,2,6,-4,-1,-4,9),3,3,byrow=T)
 A

 EA <- eigen(A)
 EA

 SVDA <- svd(A)
 SVDA  

#-----------------------------------------
#  An example of a square symmetric matrix 
#  that is not positive definite
#-----------------------------------------

 W<-matrix(c(4,2,-1,2,6,-4,-1,-4,-9),3,3,byrow=T)

 W

 EW <- eigen(W)
 EW

 t(EW$vectors)%*%EW$vectors
 
 SVDW <- svd(W)

 SVDW


#--------------------------------------
#  Inverse of a matrix 
#--------------------------------------

 A <- matrix(c(1.96,.72,.72,1.54),2,2,byrow=T)

 Ainv <- solve(A)
 Ainv

 A%*%Ainv

#--------------------------------------
#  Use the spectral decomposition
#  to compute the inverse of a matrix
#--------------------------------------

 Aev<-eigen(A)$vectors
 Aeval<-eigen(A)$values
 Ainv2<-Aev%*%solve(diag(Aeval))%*%t(Aev)
 Ainv2

#------------------------------------------
#    Solutions to linear equations
#------------------------------------------

 x<-c(1,1)
 x

 b<-solve(A,x)
 b
 
 








