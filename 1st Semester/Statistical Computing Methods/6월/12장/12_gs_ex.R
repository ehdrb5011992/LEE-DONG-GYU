# ex1
n.sim <- 10000
rho <- 0.5
x <- y <- NULL
x[1] <- rnorm(1, 0, 1)
y[1] <- rnorm(1, 0, 1)

for (i in 2:n.sim) {
  y[i] <- rnorm(1, rho * x[i-1], 1 - rho^2)
  x[i] <- rnorm(1, rho * y[i]  , 1 - rho^2)
}

# ex2
n.sim <- 10000
n <- 15
a <- 3
b <- 7
x <- theta <- NULL
theta[1] <- rbeta(1, a, b)
x[1] <- rbinom(1, n, theta[1])

for (i in 2:n.sim) {
  x[i] <- rbinom(1, n, theta[i-1])
  theta[i] <- rbeta(1, a + x[i], b + n - x[i])
}
