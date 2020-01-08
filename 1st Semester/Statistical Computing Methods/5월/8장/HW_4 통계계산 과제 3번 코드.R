#install.packages("kernlab")
library(kernlab)

rm(list=ls())
gc()

#simulation
set.seed(314)
n <- 30 # sample size
p <- 5   # predictor dimension
C <- 2   # cost for relaxation

# predictor
x <- matrix(rnorm(n*p), n, p)
# true desision function
true.beta0<-1
true.beta<-rep(3,p)
fx <- true.beta0+x%*%true.beta

# error
e <- rnorm(n,0,0.1)

# output label
y <- c(fx + e)

my_kqr <- function(x,y,tau,C) { # x is data, y is {-1,1} and tau is a number in (0,1)

  if (tau > 1 & tau <0) stop("Input 0 <= tau <= 1")
  
  n <- nrow(x) # sample size
  p <- ncol(x) # predictor dimension
  K <- x %*% t(x) 
  lambda <- 1 / C
  
  c <- -lambda * y
  H <- K
  A <- t(rep(1,n))
  b <- 0
  l <- -(1-tau) * rep(1,n)
  u <- tau * rep(1,n)
  r <- 0
  
  
  obj <- ipop(c, H, A, b, l, u, r)
  theta <- primal(obj)
  
  sv.id <- which(-(1-tau)+1.0e-3 < theta & theta < (tau - 1.0e-3))
  beta <- C * t(x) %*% theta 
  
  temp <- y[sv.id] -  x[sv.id,] %*% beta
  beta0 <- mean(temp)
  
  
  return(list( beta0 = beta0 , beta = beta ))
  
}

my_kqr(x,y,0.5,4)














