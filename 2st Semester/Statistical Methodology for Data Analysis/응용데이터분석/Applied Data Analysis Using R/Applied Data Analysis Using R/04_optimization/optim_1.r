# example 1. Poisson observations
x <- c(3,10,5,4)
n <- length(x)
n.log.lik <- function(mu) n*mu - sum(x)*log(mu)
optim(n.log.lik, method="L-BFGS-B", par =1, lower=0, upper=100) 

# example 2. Weibull observations
x <- c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)
n <- length(x)
n.log.lik <- function(theta) {
   theta.1 <- theta[1]; theta.2 <- theta[2]
   temp.1 <- -n*log(theta.1/theta.2) - (theta.1-1)*sum(log(x/theta.2))
   temp.2 <- sum((x/theta.2)^theta.1)     
   return( temp.1 + temp.2)
}
optim(fn=n.log.lik, par=c(1,10), method="L-BFGS-B", lower=c(0,0)) 
optim(fn=n.log.lik, par=c(1,10), method="Nelder-Mead") 

# example 1 (continued)
library(stats4)
x <- c(3,10,5,4)
n <- length(x)
n.log.lik <- function(mu) n*mu - sum(x)*log(mu)
fitted <- mle(n.log.lik, start=list(mu=1), nobs=n)
logLik(fitted)
summary(fitted)
confint(fitted)

# end
