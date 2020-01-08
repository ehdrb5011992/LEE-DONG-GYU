rm(list=ls())
gc()
set.seed(1)




my.poi <- function(X, y, init = NULL, max.iter = 100, eps = 1.0e-5)
{
  if (is.null(init)) init <- rep(0, ncol(X))
  beta <- init
  
  for (iter in 1: max.iter) {
    
    eta <- X %*% beta
    lambda <- exp(eta)
    w <- c(lambda)
    z <- X %*% beta + (y-lambda)/w
  
    tilde.X <- X * sqrt(w)
    tilde.y <- z * sqrt(w)
    qr.obj <- qr(tilde.X)
    Q<-qr.Q(qr.obj) ; R<-qr.R(qr.obj)
    
    new.beta <- c(backsolve(R,t(Q) %*% tilde.y))
    # = new.beta <- backsolve(R,qr.qty(qr.obj,tilde.y))
    # = new.beta <- backsolve(qr.obj$qr, qr.qty(qr.obj, tilde.y)) #solution of normal equation
    
    if ( max(abs(new.beta - beta)) < eps  ) break
    beta<-new.beta
      
  }
    
  if (iter == max.iter) warning("Algorithm may not be converged!")
  
  obj <- list(est = c(beta), iterations = iter)
  return(obj)
}

#ex)

n <- 100 # sample size
p <- 3   # predictor dimension

x <- matrix(rnorm(n*p), n, p) # generate predictor
X <- cbind(rep(1, n), x)      # design matrix
beta <- rep(1, p+1) # true beta
eta <- X %*% beta   
lambda <- exp(eta) 
y <- rpois(n,lambda)  



#  my function based on NR (IWLS)
obj1 <- my.poi(X, y)
hat.beta1 <- obj1$est

# check with R-built-in function, glm  
obj2 <- glm(y ~ x, family = "poisson")
hat.beta2 <- coefficients(obj2)

# confirm
print(head(cbind(hat.beta1, hat.beta2))) ; obj1$iterations



