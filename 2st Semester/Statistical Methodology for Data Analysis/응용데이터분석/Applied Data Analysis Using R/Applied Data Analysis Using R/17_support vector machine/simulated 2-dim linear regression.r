# simulated 2-dim linear SVM regression

set.seed(12345)
x <- rnorm(100)
y <- 0.8*x + rnorm(100,0,0.6)  # runif(100,-1,1)
sd(x)
sd(y)
windows(height=7.5,width=7)
plot(y ~ x, main="simulated bivariate data",xlim=c(-3,3),ylim=c(-3,3))
abline(c(0,0.8),col="blue")
abline(c(-1,0.8),col="red",lty="dotted")
abline(c(1,0.8),col="red",lty="dotted")
for (i in 1:100){
  if (y[i]-0.8*x[i] >  1) segments(x[i],y[i],x[i],0.8*x[i]+1)
  if (y[i]-0.8*x[i] < -1) segments(x[i],y[i],x[i],0.8*x[i]-1)
}

library(e1071)
svm.model <- svm(y ~ x, kernel="linear", epsilon=1, scale=F)  # kernel="radial", epsilon=1, gamma=0.5
summary(svm.model)

windows(height=7.5,width=7)
plot(y ~ x, main="simulated bivariate data",xlim=c(-3,3),ylim=c(-3,3))
par(new=T)
plot(svm.model$fitted ~ x, main="",xlim=c(-3,3),ylim=c(-3,3),xlab="",ylab="",col="red", pch=20)
points(x[svm.model$index],y[svm.model$index],pch=20)

# end