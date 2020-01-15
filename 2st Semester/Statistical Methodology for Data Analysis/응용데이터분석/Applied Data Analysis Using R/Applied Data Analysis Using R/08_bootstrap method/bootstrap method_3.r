# difference of two sample means in the final two series of the gravity data.

# the case of two simulated gamma samples

library(boot)

set.seed(123)
x <- rgamma(100,5,0.8)
y <- rgamma(100,5,1.0)
x.and.y <- data.frame(x, y)
z <- stack(x.and.y)
t.test(x, y, var.equal=T)

diff.means <- function(data, n1, indices) {
	m1 <- mean(data[indices[indices <= n1],1])
	m2 <- mean(data[indices[indices >  n1],1])
	c(m1 - m2)
}

z.boot <- boot(data=z, statistic=diff.means, n1=100, R=1000, strata=z[,2])
boot.ci(z.boot, type = c("perc", "bca"))

# end