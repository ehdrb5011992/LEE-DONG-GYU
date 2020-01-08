rm(list = ls())

# soft-thresholding operator
S <- function(z, lambda) 
{
  (z - lambda) * (z > lambda) + 
    (z + lambda) * (z < -lambda) + 
    0 * (abs(z) <= lambda)
}

# LASSO update function for CD 
lasso.update <- S

# SCAD update function for CD
scad.update <- function(z, lambda, a = 3.7)
{
  if (abs(z) < 2 * lambda){
    S(z, lambda)
  } else if ((2 * lambda < abs(z)) & (abs(z) <= a * lambda)){
    S(z, a * lambda / (a - 1)) / (1 - 1 / (a - 1))
  } else z
}
#my.scad.update <- function (z,lambda,a=3.7) {
#  
#  
#  S(z,lambda) * (abs(z) < 2*lambda) +
#    S(z, a * lambda / (a - 1)) / (1 - 1 / (a - 1)) * (2 * lambda < abs(z)) * (abs(z) <= a * lambda) +
#    z * (abs(z) > a * lambda)
#  
#}

# MCP update function for CD
mcp.update = function(z, lambda, gamma)
{
  if (abs(z) <= gamma * lambda){
    S(z, lambda) / (1 - 1 / gamma)
  } else z
}
#my.mcp.update = function(z, lambda, gamma)
#{
#    S(z, lambda) / (1 - 1 / gamma) * (abs(z) <= gamma * lambda) +
#      z * (abs(z) > gamma * lambda )
#}



# coordinate decent algorithm
ncv <- function(x, y, lambda, type = "lasso", power = 2, a = 3.7, gamma = 3.7, init = rep(0, p), max.iter = 100, eps = 1.0e-8)
{
  n <- length(y)
  p <- ncol(x)
  
  # standardize
  u <- y - mean(y)
  z <- t(t(x) - apply(x, 2, mean))
  norm.z <- apply(z^2, 2, mean)
  z <- t(t(z)/sqrt(norm.z))
  
  # initialize beta
  beta <- init
  
  # residual
  r <- (u - z %*% beta)
  
  if (type == "lasso") {
    update.ft <- function(z) lasso.update(z, lambda)
  } else if (type == "scad") {
    update.ft <- function(z) scad.update(z, lambda, a)
  } else if (type == "mcp") {
    update.ft <- function(z) mcp.update(z, lambda, gamma)
  } else stop("type should be lasso, scad, or mcp!")
  
  # start update
  for (t in 1:max.iter)
  {
    new.beta <- beta
    for (j in 1:p)
    {
      zj <- 1/n * crossprod(z[,j],  r) + beta[j]# inner product
      new.beta[j] <- update.ft(zj)              # soft thresholding
      r <- r - (new.beta[j] - beta[j]) * z[,j]  # update full residuals
    }
    if (max(abs(beta - new.beta)) < eps) break
    beta <- new.beta
  }
  
  # transform back
  beta.lasso <- beta / sqrt(norm.z)
  intercept <- mean(y) - sum(apply(x, 2, mean) * beta.lasso)
  
  index <- which(abs(beta.lasso) > eps) #surviving betas
  beta.info <- beta.lasso[index]
  
  obj = list(intercept = intercept,
             beta = beta.info,
             index = index)
}

# example
set.seed(1)
n <- 100
p <- 5

d <- 2
beta <- c(rep(1, d), rep(0, p-d))

x <- matrix(rnorm(n*p), n, p)
e <- rnorm(n)
y <- x %*% beta + e

lambda <- .1
# lasso
obj1 <- ncv(x, y, lambda, type = "lasso")
# scad
obj2 <- ncv(x, y, lambda, type = "scad")
# mcp
obj3 <- ncv(x, y, lambda, type = "mcp")

print(obj1)
print(obj2)
print(obj3)

