set.seed(1)
a <- 2.7
b <- 6.3

n.sim <- 10000
X <- NULL
ac <- NULL
X[1] <- runif(1)
ac[1] <- 1
for (i in 2:n.sim) {
  Y <- runif(1)
  rho <- dbeta(Y, a, b) / dbeta(X[i-1], a, b)
  ac[i] <- runif(1) < rho
  X[i] <- X[i-1] + (Y - X[i-1]) * ac[i]
}

cat("acceptance rate = ", mean(ac), "\b")
# distribution test
obj <- ks.test(jitter(X), rbeta(n.sim, a, b))
print(obj)

plot(X[(n.sim-500):n.sim], main = "Part of trace plot", type = "l")

hist(X, probability = T, breaks = 50, main = "MH algoritghm")
lines(seq(0, 1, length = 1000), dbeta(seq(0, 1, length = 1000), a, b), col = 2, lty = 2)


acf(X, main = "MH Algorithm")
