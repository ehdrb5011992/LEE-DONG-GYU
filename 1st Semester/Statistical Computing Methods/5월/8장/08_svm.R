rm(list = ls())
library(quadprog)
library(svmpath)

set.seed(1)

n <- 100 # sample size
p <- 2   # predictor dimension
C <- 1   # cost for relaxation

# predictor
x <- matrix(rnorm(n*p), n, p)
# true parameters
true.beta0 <- 0
true.beta <- rep(2, p)

# true desision function
fx <- log(x[,1]^2 + x[,2]^2)

# error
e <- rnorm(n)

# output label
y <- c(sign(fx + e))

par(mfrow = c(2,2))
# plot
plot(x[,1], x[,2], type = "n", xlab = "x1", ylab = "x2", 
     main = "Support Vector Machine")
points(x[y == 1, 1], x[y == 1, 2], pch = 1, col = 2)
points(x[y != 1, 1], x[y != 1, 2], pch = 3, col = 4)


# Qquadratic Programming for SVM
# radial
q <- 1/p
normx <- drop((x^2) %*% rep(1, p))
a <- x %*% t(x)
a <- (-2 * a + normx) + outer(rep(1, n), normx, "*")
K <- exp(-a * q)

# linear
#K <- x %*% t(x)


K.star <- K * (outer(y, y)) + 1.0e-8 * diag(rep(1, n))

d <- rep(1, n)
A <- cbind(y, diag(n), -diag(n))
b <- c(0, rep(0, n), rep(-C, n))
obj <- solve.QP(K.star, d, A, b, meq = 1)
alpha <- obj$solution # dual solution

# indices for sv
sv.id <- which(1.0e-3 < alpha & alpha < (C - 1.0e-3))

# beta0
temp <- y[sv.id] - t(y * K[,sv.id,drop = F]) %*% alpha
beta0 <- mean(temp)

hat.fx <- beta0 + (y * K) %*% alpha

boxplot(hat.fx[y == 1], hat.fx[y != 1])

table(y, sign(hat.fx))


# solution path
obj <- svmpath(x, y, kernel = radial.kernel)

lambda <- obj$lambda
alpha0 <- obj$alpha0
alpha  <- obj$alpha

plot(lambda, alpha0, col = 2, lty = 2, type = "l",
     main = "Regularization path (intercept)")
abline(v = lambda, lty = 2, col = "gray80")

plot(0, 0, type = "n", xlab = "lambda", ylab = "alpha", 
     xlim = quantile(lambda, c(0, 1)), ylim = c(0, 1),
     main = "Regularization path")
abline(v = lambda, lty = 2, col = "gray80")

for (j in 1:nrow(alpha))  lines(lambda, alpha[j,], col = 2, lty = 2)


