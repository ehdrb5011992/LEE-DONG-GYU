rm(list = ls())

setwd('~/Dropbox/teaching/201901_ST509/r-code')
source('02_fn_ge.R')

set.seed(2)
n <- 5
A <- matrix(rnorm(n^2), n, n)
b <- matrix(rnorm(n), n, 1)

x1 <- solve(A, b)
x2 <- solve.ge(A, b)

A %*% x1
A %*% x2
print(cbind(x1, x2))