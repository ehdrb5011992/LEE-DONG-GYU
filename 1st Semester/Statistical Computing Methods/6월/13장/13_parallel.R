rm(list = ls())

set.seed(1)
n <- 1000
B <- 200

x <- rnorm(n)
beta0 <- 0
beta1 <- 1
eta <- beta0 + beta1 * x
p <- exp(eta)/(exp(eta) + 1)
y <- rbinom(n, 1, p)

# function for bootsrap samples
fx <- function(b) {
  set.seed(b)
  id.bt <- sample(n, replace = T)
  y.bt <- y[id.bt]
  x.bt <- x[id.bt]
  coef(glm(y.bt ~ x.bt, family = "binomial"))
}

# bootstrap samples 
# for (not parallelized)
tic1 <- Sys.time()
beta.bt1 <- matrix(0, B, 2)
for (b in 1:B) {
  beta.bt1[b,] <- fx(b)
}
toc1 <- Sys.time()
print(toc1 - tic1)


# parallel package
library(parallel)
n.cores <- detectCores()
n.cores

tic2 <- Sys.time()
beta.bt2 <- mclapply(1:B, fx, mc.cores = n.cores)
toc2 <- Sys.time()
print(toc2 - tic2)

# foreach package
library(foreach)
library(doParallel)
registerDoParallel(n.cores)  
tic3 <- Sys.time()
beta.bt3 <- foreach (b=1:B) %dopar% {
            fx(b)
}
toc3 <- Sys.time()
print(toc3 - tic3)


