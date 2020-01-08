rm(list = ls())

set.seed(1)
# two-sample mean comparison
n <- 50
B <- 500

mu1 <- 0
mu2 <- 1

x <- rnorm(n, mu1, 2)
y <- rnorm(n, mu2, 4)

# true.value
pv <- 1 - pnorm((mean(y) - mean(x)) / sqrt(4/n + 16/n))

# bootstrap test
pool <- c(x, y)
count <- 0
t <- mean(y) - mean(x)
t.bt <- NULL
for (b in 1:B) {
  id.bt <- sample(2 * n, replace = T)
  x.bt <- pool[id.bt[1:n]]
  y.bt <- pool[-id.bt[1:n]]
  t.bt[b] <- mean(y.bt) - mean(x.bt)
}

pv.bt <- mean(t.bt >= t)


print(pv)
print(pv.bt)



# one-sample mean comparison
B <- 500
n <- 20

mu <- 1
mu0 <- 0
sigma <- 4
x <- rnorm(n, mu, sigma)
pv <- 1 - pnorm(sqrt(n) * (mean(x) - mu0)/sigma)

# bootstrap test
x.tilde <- x - mean(x) + mu0
t <- sqrt(n) * (mean(x) - mu0)/sd(x)
t.bt <- NULL
for (b in 1:B) {
  id.bt <- sample(n, replace = T)
  x.bt <- x.tilde[id.bt]
  t.bt[b] <- sqrt(n) * (mean(x.bt) - mu0)/sd(x.bt)
}

pv.bt <- mean(t.bt >= t)

pv
pv.bt