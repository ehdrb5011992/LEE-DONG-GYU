# EM algorithm for two-component Gaussian Mixture
set.seed(3)
n <- 500

p <- 0.5

m1 <- -3
s1 <- 1

m2 <- 3
s2 <- 2
true.theta <- c(m1, m2, s1, s2, p)

# data generation
z <- rbinom(n, 1, p)
x1 <- rnorm(n, m1, s1)
x2 <- rnorm(n, m2, s2)

y <- z * x1 + (1-z) * x2
plot(density(y))

max.iter <- 100
eps <- 1.0e-5

# initialization
m1 <- 1
m2 <- 2
s1 <- 1
s2 <- 2
p <- 0.3

theta <- c(m1, m2, s1, s2, p)
print(theta)


for (iter in 1:max.iter)
{
  p1 <- p
  p2 <- 1-p1
  
  # compute weight
  f1 <- exp(log(p1) + dnorm(y, m1, s1, log = T))
  f2 <- exp(log(p2) + dnorm(y, m2, s2, log = T))
  
  w1 <- f1/(f1 + f2)
  w2 <- f2/(f1 + f2)
  
  # update mixture proportion
  p <- mean(w1)
  
  # update mean
  m1 <- sum(w1 * y)/sum(w1)
  m2 <- sum(w2 * y)/sum(w2)
  
  # update standard deviation
  s1 <- sqrt(sum(w1 * (y - m1)^2)/sum(w1))
  s2 <- sqrt(sum(w2 * (y - m2)^2)/sum(w2))
  
  
  new.theta <- c(m1, m2, s1, s2, p)
  
  if (max(abs(new.theta - theta)) < eps) break
  theta <- new.theta
}




print(cbind(true.theta, new.theta))
