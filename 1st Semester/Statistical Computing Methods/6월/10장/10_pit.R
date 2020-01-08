# set random seed
set.seed(1)

par(mfrow = c(2,2)) # graphical parameter

n <- 50000 # sample size

###################
# 1. Exp (lambda) #
###################

lambda = 2 # rate parameter (mean = 1/lambda)

grid <- seq(0, 6/lambda, length = 1000) # grod for the graph


# 1-1. x: built-in function
x <- rexp(n, lambda)

# histogram of x
hist(x, col = rgb(0, 1, 0, alpha = 0.4), probability = T, 
     breaks = 50, xlim = c(0,6/lambda),
     xlab = "",
     main = paste("Built-in: Exp(", lambda, ") ", sep = ""))
lines(grid, dexp(grid, lambda), col = 2, lwd = 2) # true density

# 1-2. y: use PIT
t <- runif(n)
y <- -log(1-t)/lambda

hist(x, col = rgb(0, 1, 1, alpha = 0.4), probability = T, 
     breaks = 50, xlim = c(0,6/lambda),
     xlab = "",
     main = paste("PIT: Exp(", lambda, ") ", sep = ""))
lines(grid, dexp(grid, lambda), col = 2, lwd = 2) # true density



#########################
# 2. Normal (mu, sigma) #
#########################

mu <- 0 # mean
sigma <- 1 # sd

grid <- seq(mu - 3.5*sigma, mu + 3.5*sigma, length = 1000) 

# 2-1. x: built-in function
x <- rnorm(n, mu, sigma)

hist(x, col = rgb(0, 1, 0, alpha = 0.4), probability = T,
     breaks = 50, xlim = c(mu - 3.5*sigma, mu + 3.5*sigma),
     xlab = "", 
     main = paste("Built-in: N(", mu, ", ", sigma^2, ")", sep = ""))
lines(grid, dnorm(grid, mu, sigma), col = 2, lwd = 2)


# 2-2. y: use PIT
t <- runif(n)
y <- qnorm(t, mu, sigma)

hist(y, col = rgb(0, 1, 1, alpha = 0.4), probability = T,
     breaks = 50, xlim = c(mu - 3.5*sigma, mu + 3.5*sigma),
     xlab = "", 
     main = paste("PIT: N(", mu, ", ", sigma^2, ")", sep = ""))
lines(grid, dnorm(grid, mu, sigma), col = 2, lwd = 2)




