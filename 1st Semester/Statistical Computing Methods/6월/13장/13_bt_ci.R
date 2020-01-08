rm(list = ls())

set.seed(1)
n <- 100
B <- 500
alpha <- 0.05

mu <- 1

x <- rnorm(n, mu, 1)
# CI for median
theta <- median(x)
theta.sd <- 1.253 * sd(x)/sqrt(n)
# bootstrap CI
theta.bt <- theta.sd.bt <- t.bt <- NULL
for (b in 1:B) {
  id.bt <- sample(n, replace = T)
  x.bt <- x[id.bt]
  theta.bt[b] <- median(x.bt)
  theta.sd.bt[b] <- 1.253 * sd(x.bt)/sqrt(n)
  t.bt[b] <- (theta.bt[b] - theta)/theta.sd.bt[b]
}

# percentile 
ci1 <- quantile(theta.bt, c(alpha/2, 1-alpha/2))
ci1
# relected percentile
ci2 <- 2*theta - ci1[2:1]
ci2
# bootstrap-t
ci3 <- theta - theta.sd * quantile(t.bt, c(1-alpha/2, alpha/2))
ci3



