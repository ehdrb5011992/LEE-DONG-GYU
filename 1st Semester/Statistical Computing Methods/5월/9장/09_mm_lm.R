rm(list = ls())
library(quantreg)

mm.ls <- function(x, y, max.iter = 100, eps = 1.0e-8)
{
  beta <- rep(0, p) 
  iter <- 1
  for (iter in 1:max.iter)
  {
    alpha <- abs(x) / apply(abs(x), 1, sum)
    r <- c(y - x %*% beta)
    temp1 <- apply(x * r, 2, sum)
    temp2 <- apply(x^2/alpha, 2, sum) 
    beta.new <- beta + temp1/temp2
    if (max(abs(beta.new - beta)) < eps) break
    beta <- beta.new
  }
  return(beta.new)
}


mm.md <- function(x, y, max.iter = 100, eps = 1.0e-8)
{
  beta <- rep(0, p) 
  iter <- 1
  for (iter in 1:max.iter)
  {
    r <- c(y - x %*% beta)
    w <- 1/abs(r + 1.0e-12) # avoid overflow
    
    tilde.x <- x * sqrt(w)
    tilde.y <- y * sqrt(w)
    qr.obj <- qr(tilde.x)
    beta.new <- backsolve(qr.obj$qr, qr.qty(qr.obj, tilde.y))
    if (max(abs(beta.new - beta)) < eps) break
    beta <- beta.new
  }
  return(c(beta.new))
}

set.seed(1)

n <- 100
p <- 5

beta <- rep(1:p)

x <- matrix(rnorm(n*p), n, p)
e <- rnorm(n, 0, 0.2)

y <- x %*% beta + e

###########################
# least square regression #
###########################
# existing function
beta.ls <- coef(lm(y ~ -1 + x))
# mm algorithm
beta.ls.mm <- mm.ls(x, y)

print(round(cbind(beta.ls, beta.ls.mm), 5))

#####################
# median regression #
#####################
# existing function
beta.md <- coef(rq(y ~ -1 + x))
# mm algorithm
beta.md.mm <- mm.md(x, y)

print(round(cbind(beta.md, beta.md.mm), 5))
