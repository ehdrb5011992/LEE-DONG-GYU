# nonlinear SVM regression

set.seed(12345)
x <- rnorm(100)
y.fit <- 0.8*x^2
y <- y.fit + rnorm(100,0,0.6)  # runif(100,-1,1)

windows(height=7.5,width=7)
plot(y ~ x, main="simulated bivariate data 2",xlim=c(-3,3),ylim=c(-2,6))
par(new=T)
plot(y.fit ~ x, main="",xlim=c(-3,3),ylim=c(-2,6),xlab="",ylab="",col="red", pch=20)
index <- (1:100)[abs(y-y.fit)>=1]
points(x[index],y[index],pch=20,col="blue")

library(e1071)
svm.model <- svm(y ~ x, gamma=0.5, epsilon=1, scale=F)  # kernel="radial", epsilon=1, gamma=0.5
summary(svm.model)

windows(height=7.5,width=7)
plot(y ~ x, main="simulated bivariate data 2",xlim=c(-3,3),ylim=c(-2,6))
par(new=T)
plot(svm.model$fitted ~ x, main="",xlim=c(-3,3),ylim=c(-2,6),xlab="",ylab="",col="red", pch=20)
points(x[svm.model$index],y[svm.model$index],pch=20)

# end
