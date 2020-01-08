# computing Prob(Z > 4.5)

# true probability
true <- exp(pnorm(-4.5, log = T))
print(true)

# 1. nieve MC estimates
set.seed(1)
n <- 1000

y <- rnorm(n)
est1 <- mean(y > 4.5)
print(est1)

# 2. importance sampling from exp(1) truncated at 4.5
y <- rexp(n) + 4.5 
weight <- dnorm(y)/dexp(y - 4.5)
est2 <- mean(weight)
print(est2)

# check convergence
par(mfrow = c(1,1))
plot(cumsum(weight)/1:n, type = "l", ylab = "est", xlab = "# of simulation")
abline(h = true, col = 2)






