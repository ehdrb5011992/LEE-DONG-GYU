rm(list = ls())

my.logit <- function(X, y, init = NULL, max.iter = 100, eps = 1.0e-5)
{
  if (is.null(init)) init <- rep(0, ncol(X))
  beta <- init
  for (iter in 1:max.iter)
  {
    
    eta <- X %*% beta
    p <- exp(eta)/ (1 + exp(eta))
    w <- c(p * (1 - p))
    z <- X %*% beta + (y-p)/w
    
    #new.beta <- solve(crossprod(tilde.X, tilde.X)) %*% crossprod(tilde.X, tilde.z)
    
    tilde.X <- X * sqrt(w)
    tilde.z <- z * sqrt(w)
    qr.obj <- qr(tilde.X)
    new.beta <- backsolve(qr.obj$qr, qr.qty(qr.obj, tilde.z))
    
    if (max(new.beta - beta) < eps) break
    beta <- new.beta
  }
  if (iter == max.iter) warning("Algorithm may not be converged!")
  obj <- list(est = c(beta), iterations = iter)
}

set.seed(1)

n <- 100 # sample size
p <- 3   # predictor dimension

x <- matrix(rnorm(n*p), n, p) # generate predictor
X <- cbind(rep(1, n), x)      # design matrix

beta <- rep(1, p+1) # true beta

eta <- X %*% beta   # true eta (linear term)
  
pi <- exp(eta)/(1 + exp(eta)) # pi = mu = E(y|x)
y <- rbinom(n, 1, pi)         # generate reponse
  
#  my function based on NR (IWLS)
obj1 <- my.logit(X, y, max.iter = 100)
hat.beta1 <- obj1$est

# check with R-built-in function, glm  
obj2 <- glm(y ~ x, family = "binomial")
hat.beta2 <- coefficients(obj2)

# compare
print(head(cbind(hat.beta1, hat.beta2)))

