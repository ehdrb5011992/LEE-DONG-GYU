rm(list = ls())

# bias-corrected estimator of maximum
n.sim <- 500
set.seed(1)
n <- 30
theta <- theta.jk <- NULL
for (iter in 1:n.sim)
{
  u <- runif(n) 
  theta[iter] <-  max(u)
  
  # Jackknife bias corrected estimator
  theta.i <- NULL
  for (i in 1:n) theta.i[i] <- max(u[-i])
  theta.jk[iter] <- theta[iter] - (n-1) * (mean(theta.i) - theta[iter])
}

hist(theta, xlab = expression(theta), main = "")
abline(v = 1, col = 2)
abline(v = mean(theta), col = 4, lty = 2, lwd = 2)

hist(theta.jk, xlab = expression(theta), main = "")
abline(v = 1, col = 2)
abline(v = mean(theta.jk), col = 4, lty = 2, lwd = 2)


