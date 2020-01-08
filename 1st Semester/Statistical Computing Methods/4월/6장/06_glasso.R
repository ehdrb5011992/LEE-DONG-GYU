rm(list = ls())
gc()
source("C:\\Users\\82104\\Desktop\\대학원\\1학년 1학기\\통계계산 방법론\\4월\\5장\\05_cd_lasso(1).R")
library(glasso)


set.seed(100)
n <- 50
p <- 5
x<-matrix(rnorm(n*p),ncol = p)
s<- var(x)

lambda <- 0.01

eps <- 1.0e-3

# Initialization
W <- s + diag(rep(lambda*n, p))


# initialization 
for (i in 1:100)
{
  W.old <- W
  beta <- matrix(0, p-1, p)
  for (j in 1:p)
  {
    W11 <- W[-j,-j,drop = F]
    
    Wsqrt <- chol(W11)  # W_11^1/2
    Wsqrt.i <- t(backsolve(Wsqrt, diag(p-1))) # Its inverse
    
    beta[,j] <- cd.lasso(x = Wsqrt, y = Wsqrt.i %*% s[-j,j], lambda)
    W[-j,j] <- W[j,-j] <- c(W11 %*% beta[,j])
    
    
  }
  if (max(abs(W - W.old)) < eps) break
}

# compute precision matrix theta
theta <- matrix(0, p, p) # storage for Theta
for (j in 1:p)
{
  theta[j,j] <- 1/(W[j,j] - W[-j,j] %*% beta[,j])
  theta[j,-j] <- theta[-j,j] <- c(-beta[,j]  * theta[j,j])
}

print(theta)
