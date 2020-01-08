# set soft-thresholding function
rm(list=ls())
gc()
S <- function(z, lambda) {
  (z - lambda) * (z > lambda) + 
    (z + lambda) * (z < -lambda) + 
    0 * (abs(z) <= lambda)
}

set.seed(1)

n <- 10
p <- 5   # try 5, 20, 50

x <- matrix(rnorm(n*p, 1, 1), n, p) # predictor
e <- rnorm(n, 0, 0.5) # noise

true.beta <- rep(0, p + 1)
true.beta[1] <- 1 # intercept
true.beta[2:(p+1)] <- c(rep(1, 3), rep(0, p-3)) # coefficients

y <- true.beta[1] + x %*% true.beta[-1] + e # response

# lm
est0 <- coef(lm(y ~ x))

lambda <-0.1

# CD algorithm for lasso
# marginal standardization of x
bar.x <- apply(x, 2, mean)
s <- sqrt(apply(x, 2, var) * (n-1)/n)
z <- t((t(x) - bar.x)/s) # 지린다...

# centering of y
u <- (y - mean(y))

# initialization
beta <- coef(lm(u ~ z - 1))
r <- u - z %*% beta # r <- lm(u~z-1) 's residuals

for (iter in 1:100) {
   new.beta <- beta
   for (j in 1:p) {
      temp <- beta[j] + crossprod(z[,j], r)/n
      new.beta[j] <- S(temp, lambda/s[j]) 
      r <- r - (new.beta[j] - beta[j]) * z[,j]
   }
  delta <- max(abs(new.beta - beta))
  if (delta < 1.0e-3) break
  beta <- new.beta
}


beta <- new.beta/s 
beta0 <- mean(y) - crossprod(beta, bar.x)
(( est1 <- c(beta0, beta) ))

ee <-glmnet(x,y,alpha=1,lambda=0.1,family="poisson")
coef(ee)
#my.reg.enet(x,y,alpha=1,lambda=0.1)

#print(round(cbind(true.beta, est1), 3))

