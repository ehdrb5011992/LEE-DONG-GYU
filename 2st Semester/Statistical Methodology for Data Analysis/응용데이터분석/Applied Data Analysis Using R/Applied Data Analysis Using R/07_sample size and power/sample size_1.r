# sample size and power

mu <- 0.4; sigma <- 1
n <- c(10,40)
round(1-pnorm(1.645-sqrt(n)*mu/sigma, 0, 1), 3)

library(pwr)
pwr.norm.test(d=0.4, n=c(10,40), sig.level=0.05, alternative="greater")

# 1. normal power curve
mu <- seq(0,1,0.05)
n <- 40
pwr <- pwr.norm.test(d=mu, n=n, sig.level=0.05, alternative="greater")$power
plot(mu, pwr, type="l", main=paste("normal power curve, n =",n))
abline(h = 0.8, col="blue", lty="dotted", lwd=1.2)

pwr.norm.test(d=0.4, power=0.80, sig.level=0.05, alternative="greater")

# end