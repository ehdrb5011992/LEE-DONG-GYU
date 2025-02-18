rm(list = ls())

mcmc.logit <- function(x, y, init, n.sample = 10000, step = 0.3){
  post.beta <- matrix(0, n.sample, p)
  ac.ratio <- rep(0, n.sample)
  
  prior.m <- 10
  prior.s <- 1000 # for vague prior
  
  # intialize
  post.beta[1,] <- beta <- init
  eta <- x %*% beta
  pi <- exp(eta)/(1 + exp(eta))
  
  log.prior <- sum(dnorm(beta, prior.m, prior.s, log = T))
  log.like  <- sum(y * log(pi) +  (1 - y) * log(1 - pi))
  
  iter <- 2
  for (iter in 1:n.sample){
    
    # candidate
    beta.new <- beta + rnorm(p, 0, step) 
    eta.new <- x %*% beta.new
    pi.new  <- exp(eta.new)/(1 + exp(eta.new))
    
    # prior
    log.prior.new <- sum(dnorm(beta.new, prior.m, prior.s, log = T))
    
    # liklihood
    log.like.new <- sum(y * log(pi.new) + (1 - y) * log(1 - pi.new))
    
    # ratio
    temp <- exp((log.like.new + log.prior.new) - (log.like + log.prior))
    rho <- min(1, temp)
    
    if (runif(1) < rho) {
      ac.ratio[iter] <- 1
      beta <- beta.new
      log.prior <- log.prior.new
      log.like  <- log.like.new
      eta <- x %*% beta
      pi <- exp(eta)/(1 + exp(eta))
    }
    post.beta[iter,] <- beta
  }
  
  obj <- list(posterior = post.beta, acpt.ratio = mean(ac.ratio))
  return(obj)
}


# Bayesian Logistic Regression
set.seed(1)

# Bayesian Logistic Regression
n <- 100 # sample size
p <- 2   # predictor dimension

x <- matrix(rnorm(n*p), n, p) # generate predictor

true.beta <- rep(-1, p) # true beta
true.eta <- x %*% true.beta   # true eta (linear term)
true.pi <- exp(true.eta)/(1 + exp(true.eta)) # pi = mu = E(y|x)
y <- rbinom(n, 1, true.pi)         # generate reponse


step <- 0.3
n.sample <- 10000
init <- rep(5, p)


# mle 
obj.mle <- glm(y ~ x - 1, family = "binomial")
mle <- coef(obj.mle)
cat("ML estimate =", mle, "\n")
print("CI based on MLE:")
print(confint(obj.mle))

# MH for Bayesian logit
obj <- mcmc.logit(x, y, init = rep(1, p), n.sample = 10000, step = 0.2)
posterior <- obj$posterior[-(1:2000),]
est <- apply(posterior, 2, mean)
cat("acptance ratio =",obj$acpt.ratio, "\n")
cat("Bayesian estimate =", est, "\n")

cr <- t(apply(posterior, 2, quantile, c(0.025, 0.975))) # Credible Region
print("CR for posteiors:")
print(cr)


# trace plot
par(mfrow = c(2,1))
for (k in 1:p){
  plot(obj$posterior[,k], type = "l", col = "gray", ylab = "posterior", xlab = "", main = "trace plot")
  abline(h = est[k], col = "blue", lwd = 2)
  abline(h = mle[k], col = "green", lty = 3, lwd = 2)
  abline(h = true.beta[k], col = "red", lty = 2, lwd = 2)
  legend("topright", c("mle", "Bayes", "True"), col = c(3,4,2), lty = c(3, 1, 2))
}


