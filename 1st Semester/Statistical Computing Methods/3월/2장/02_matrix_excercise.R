# 1
n <- 5
m <- 3
X <- outer(seq(1, 2*n - 1, 2), 0:(m-1), "^")
X

# 2
n <- 3
m <- 5
A <- matrix(c(1, -2, 1, rep(0, m-2)), n, m, byrow = T)
A

# 3
n <- 4
m <- 4
V <- exp(1 + abs(outer(1:n, 1:m, "-"))/8)
V

# 4
n <- 5
m <- 5
J <- matrix(1, n, m)
J[upper.tri(J)] <- 0
J

# 5
k <- 3
r <- 4
K <- matrix(0, 2, k*r)
K[1,] <- 1:k
K[2,] <- t(matrix(rep(1:r, k), r, k))
K