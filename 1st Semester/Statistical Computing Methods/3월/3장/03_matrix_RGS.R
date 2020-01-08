rm(list = ls())
rgs <- function(X)
{
  n <- nrow(X)
  p <- ncol(X)
  Q <- matrix(0, n, p)
  R <- matrix(0, p, p)
  
  # initialization
  R[1,1] <- sqrt(sum(X[,1]^2))
  Q[,1] <- X[,1]/R[1,1] 
 
  # update
  for (i in 2:p) {
    R[1:(i-1),i] <- crossprod(Q[,1:(i-1)], X[,i]) 
    temp <- X[,i] - t(R[1:(i-1),i] * t(Q[,1:(i-1)])) %*% rep(1, (i-1))
    #temp <- X[,i] - t(R[1:(i-1),i]) %*% t(Q[,1:(i-1)]) 내코드 : 이방법이 나음.
    R[i,i] <- sqrt(sum(temp^2))
    Q[,i] <- temp / R[i,i]
  }
  obj <- list(Q = Q, R = R)
  return(obj)
}

#1. simple illusration
X <- matrix(c(
  1, 1, 1, 1, 1, 1,
  1, 2, 3, 4, 5, 6,
  1, 4, 9,16,25,36), 6,3)

obj <- rgs(X)

(( Q <- obj$Q ))
(( R <- obj$R ))


# 2. Regression (simulation)
n <- 100 # sample size
p <- 3  # predictor dimension


X <- cbind(rep(1, n), matrix(rnorm(n * p), n, p)) # design matrix
beta <- rep(1, p+1)             # reg. coeff
eps <- rnorm(n, 0, 0.5)         # error term
y <- X %*% beta + eps           # response

# LS fit in a naive way
XX.inv1 <- solve(t(X) %*% X)
hat.beta1 <- XX.inv1 %*% t(X) %*% y

# LS fit via RGS
# Gram-Schmidt Orthogonaliation
obj <- rgs(X)
Q <- obj$Q
R <- obj$R

# XX inverse
R.inv <- backsolve(R, diag(ncol(R)))
XX.inv2 <- R.inv %*% t(R.inv)

w <- crossprod(Q, y)        # Q^T y
hat.beta2 <- backsolve(R, w) # beta estimate

