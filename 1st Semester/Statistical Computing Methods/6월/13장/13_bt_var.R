rm(list = ls())

# variance estimation of sample maximum
n.sim <- 500
B <- 200
set.seed(1)
n <- 30

v.true <- n/((n+1)^2 * (n+2)) # true.variance

theta <- theta.jk <- NULL
v.jk <- v.bt <- NULL

for (iter in 1:n.sim)
{
  u <- runif(n) 
  theta[iter] <-  max(u)
  
  # Jackknife variance
  theta.i <- NULL
  for (i in 1:n) {
    theta.i[i] <- max(u[-i])
  }
  theta.jk[iter] <- theta[iter] - (n-1) * (mean(theta.i) - theta[iter])
  theta.ps.i <- n * theta[iter] - (n-1) * theta.i
  v.jk[iter] <- mean((theta.ps.i - theta.jk[iter])^2)/(n-1)
  
  # Bootsrap variance estimator
  theta.bt <- NULL
  for (b in 1:B)
  {
    id.bt <- sample(n, replace = T)
    u.bt <- u[id.bt]
    theta.bt[b] <- max(u.bt)
  }
  v.bt[iter] <- var(theta.bt)
}
sum((v.jk - v.true)^2)
sum((v.bt - v.true)^2)

hist(v.jk, xlab = expression(theta), main = "", xlim = c(0, 0.05))
abline(v = v.true , col = 2)
abline(v = mean(v.jk), col = 4, lty = 2, lwd = 2)

hist(v.bt, xlab = expression(theta), main = "", xlim = c(0, 0.05))
abline(v = v.true , col = 2)
abline(v = mean(v.bt), col = 4, lty = 2, lwd = 2)

