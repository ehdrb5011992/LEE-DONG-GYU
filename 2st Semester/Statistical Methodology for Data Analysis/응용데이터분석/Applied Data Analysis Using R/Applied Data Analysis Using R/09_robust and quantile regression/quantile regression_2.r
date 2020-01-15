# quantile regression of Mammals

library(quantreg)
data(Mammals)
attach(Mammals)
x <- log(weight); x.1 <- x[order(x)]
y <- log(speed);  y.1 <- y[order(x)]
plot(x,y, xlab="Weight in log(Kg)", ylab="Speed in log(Km/hour)",main="Mammals")
x.2 <- x.1^2
m.rq <- rq(y.1 ~ x.1 + x.2,tau = .5)
lines(x.1, fitted(m.rq), col="blue", lwd=2)
lines(x.1, fitted(rq(y.1 ~ x.1 + x.2,tau =.25)), col="red", lwd=1)
lines(x.1, fitted(rq(y.1 ~ x.1 + x.2,tau =.75)), col="red", lwd=1)

bp <- boxplot(resid(m.rq),plot=F)
outliers <- as.numeric(labels(bp$out))  
points(x.1[outliers],y.1[outliers],pch=20)
# lines(x.1, fitted(lm(y.1 ~ x.1 + x.2,tau =.75)), col="green3", lwd=2)

# end

