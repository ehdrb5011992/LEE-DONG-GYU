library(lars)

set.seed(3)
n <- 1000
p <- 20
x <- matrix(rnorm(n * p), n, p)
beta <- c(rep(2,5), rep(0, p-5))
e <- rnorm(n, 0, 0.5)
y <- x %*% beta + e

obj <- lars(x,y,type="lasso")
par(mfrow = c(2,1))

BIC <- 2 * obj$RSS + log(n) * obj$df
AIC <- 2 * obj$RSS + 2 * obj$df

sel.BIC <- which.min(BIC)
sel.AIC <- which.min(AIC)

plot(AIC, type = "b", main = "AIC")
abline(v = sel.AIC, col = 2, lty = 2)

plot(BIC, type = "b", main = "BIC")
abline(v = sel.BIC, col = 2, lty = 2)

