# nonlinear classification using SVM

set.seed(123)
x <- matrix(rnorm(200),100,2)
grp <- ifelse(apply(x*x,1,sum) <= 1.16, 1, 2)
table(grp)

library(e1071)
y <- as.factor(grp)
svm.model <- svm(y ~ x, kernel="radial", scale=F, gamma=0.5)
summary(svm.model)

windows(height=8,width=7)
plot(x,pch=c(20,21)[grp],col=c("blue","red")[svm.model$fitted],xlim=c(-3,3),ylim=c(-3,3),main="Simulated Bivariate Data 3",xlab="x1",ylab="x2")
theta <- seq(0,1,0.01)*2*pi
r <- sqrt(1.16)
par(new=T); plot(r*cos(theta),r*sin(theta),lty="dotted",type="l",xlim=c(-3,3),ylim=c(-3,3),xlab="",ylab="")

table(svm.model$fitted)

# end