set.seed(1)
n <- 200
x <- runif(n)

beta <- 5
f <- function(x, beta) (exp(beta * x) - 1)/(exp(beta) - 1)
p <- f(x, beta)
y <- rbinom(n, 1, p)

n.sample <- 10000
step <- 0.1
M <- 10 # degress of bernstein polynomials

max.x <- 1
min.x <- 0
x.tilde <- (x - min.x)/(max.x - min.x)
W <- unlist(lapply(1:M, function(k) pbeta(x.tilde, k, M-k+1)))
Ft <- matrix(W, ncol = M)

# initialize
delta <- runif(M)

# compute gamma from delta
gamma <- delta * cumprod(1 - c(0, delta[-M]))

# p from gamma
p <- Ft %*% gamma

# likelihood
log.like <- sum(y * log(p) + (1-y) * log(1-p))

ac.ratio <- matrix(0, n.sample, M)
post.gamma <- matrix(0, n.sample, M)

for (iter in 1:n.sample) {
delta.new <- delta
gamma.new <- gamma
  for (m in 1:M)
  {
  eta <- log(delta[m]/(1 - delta[m]))
  eta.new <- eta + rnorm(1, 0, step)
  delta.new[m] <- exp(eta.new)/(1 + exp(eta.new))
  
  gamma.new <- delta.new * cumprod(1 - c(0, delta.new[-M]))
  
  # update p
  p.new <- Ft %*% gamma.new
  
  # likelihood
  log.like.new <- sum(y * log(p.new) + (1-y) * log(1-p.new))
  
  # update  
  temp <- exp(log.like.new - log.like)
  rho <- min(1, temp)
  if (runif(1) < rho) {
     ac.ratio[iter,m] <- 1
     delta[m] <- delta.new[m]
     gamma[m] <- gamma.new[m]
     log.like  <- log.like.new
     p <-Ft %*% gamma
     }
  }
post.gamma[iter,] <- gamma
}


est <- apply(Ft %*% t(post.gamma), 1, quantile, prob = c(0.025, 0.5, 0.975))

id <- order(x)

plot(x[id], est[2, id], 
     xlab = "x", ylab = "probability", col = 4, type = "l", lwd = 2,
     ylim = c(0,1))
polygon(c(x[id], rev(x[id])), c(est[1, id], rev(est[3, id])), 
        col = rgb(0,0,1, alpha = 0.22),
        border = F)
t <- seq(0, 1, length = 1000)
lines(t, f(t, beta), col = 2, lwd = 2, lty = 2)

