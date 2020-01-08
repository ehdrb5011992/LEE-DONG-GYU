# generate Beta RV using rejection sampling
set.seed(2)
a <- 2.7
b <- 6.3
# compute M
M <- optimize(f = function(x) dbeta(x, a, b), 
              interval = c(0,1), maximum = T)$objective
  
n <- 10000

y <- runif(n)
u <- runif(n)


fy <- dbeta(y, a, b)
gy <- 1
x <- y[u < (fy/M)]
ar <- mean(u < (fy/M))

par(mfrow = c(2,2))
hist(x, probability = T, breaks = 50, main = "Rejection Sampling")
lines(seq(0, 1, length = 1000), dbeta(seq(0, 1, length = 1000), a, b), col = 2, lty = 2)

x1 <- x