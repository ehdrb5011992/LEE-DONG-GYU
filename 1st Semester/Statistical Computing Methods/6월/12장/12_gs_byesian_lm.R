rm(list = ls())
set.seed(1)

# Bayesian linear model
n <- 100
x <- rnorm(n)
eps <- rnorm(n, 0, 0.5)

true.beta0 <- beta0 <- 1
true.beta1 <- beta1 <- 2

y <- beta0 + beta1 * x + eps

n.samples <- 10000

# hyperparameters
a <- 0.0001
b <- 0.0001

tau <- rep(1000, 2)
mu <- rep(0, 2)

# intial values:
sigma2 <- 1
beta <- rep(10, 2)

samples <- matrix(0, n.samples, 3)
colnames(samples) <- c("beta1", "beta2", "sigma2")

# Gibs_sampler
for(i in 1:n.samples){
  # update sigma2:
  SSE <- sum((y - beta[1] - x * beta[2])^2)
  sigma2 <- 1/rgamma(1, n/2 + a, SSE/2 + b)

  # update beta1:
  v <- n/sigma2 + 1/tau[1]^2
  m <- sum(y - x * beta[2])/sigma2 + mu[1]/tau[1]^2
  beta[1] <- rnorm(1, m/v, 1/sqrt(v))
  
  # update beta2:
  v <- sum(x^2)/sigma2 + 1/tau[2]^2
  m <- sum(x*(y-beta[1]))/sigma2 + mu[2]/tau[2]^2
  beta[2] <- rnorm(1,m/v,1/sqrt(v))
  
  samples[i,] <- c(beta, sigma2)
}

post.sample <- samples[-(1:100),]
est <- apply(post.sample, 2, mean)
print(est)

par(mfrow = c(2,1))
hist(post.sample[,1], xlab = "beta1", main = "posterior of beta1", breaks = 50)
abline(v = true.beta0, col = 2, lty = 2, lwd = 2)
abline(v = est[1], col = 4, lwd = 2)

hist(post.sample[,2], xlab = "beta2", main = "posterior of beta1", breaks = 50)
abline(v = true.beta1, col = 2, lty = 2, lwd = 2)
abline(v = est[2], col = 4, lwd = 2)

