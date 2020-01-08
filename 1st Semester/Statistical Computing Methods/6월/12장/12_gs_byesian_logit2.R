rm(list = ls())

source('~/Dropbox/teaching/201901_ST509/r-code/mcmc/12_gs_fn_mcmc_logit2.R')

set.seed(5)

# Bayesian Logistic Regression
n <- 100 # sample size
p <- 5   # predictor dimension

x <- matrix(rnorm(n*p), n, p) # generate predictor
x <- cbind(rep(1, n), x)

true.beta <- rep(-1, p+1) # true beta
true.eta <- x %*% true.beta   # true eta (linear term)
true.pi <- exp(true.eta)/(1 + exp(true.eta)) # pi = mu = E(y|x)
y <- rbinom(n, 1, true.pi)         # generate reponse


step <- 0.3
n.sample <- 10000
init <- rep(5, p)


# mle 
obj.mle <- glm(y ~ x-1, family = "binomial")
mle <- coef(obj.mle)
cat("ML estimate =", mle, "\n")
print("CI based on MLE:")
print(confint(obj.mle))

# MH for Bayesian logit
obj <- mcmc.logit2(x, y, init = rep(1, p+1), n.sample = 10000)
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



