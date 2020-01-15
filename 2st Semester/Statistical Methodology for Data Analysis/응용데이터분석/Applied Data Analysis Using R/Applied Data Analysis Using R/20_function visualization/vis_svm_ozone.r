# ozone model by svm

library(gclus)
library(kernlab)
data(ozone)
str(ozone)
x <- scale(ozone[,-1])
y <- scale(ozone[,1])
ksvm.ozone <- ksvm(x, y, kpar=list(sigma=0.125), epsilon=1, cost=1)
ksvm.ozone

windows(height=7.6, width=7)
plot(y ~ fitted(ksvm.ozone), main="ozone study",xlim=c(-4,4),ylim=c(-4,4))

n <- nrow(x)
p <- ncol(x)
half <- 0.5

par(mfrow=c(2,2))
for (j in 1:4){
  for (i in 1:n){
    min <- x[i,j]-half
    max <- x[i,j]+half
    x.new <- data.frame(matrix(rep(x[i,],9),byrow=T,nrow=9,ncol=p))
    # x.new <- cbind(rep(x[i,1],9),rep(x[i,2],9),rep(x[i,3],9),rep(x[i,4],9))
    x.new[,j] <- seq(min,max,(max-min)/8)
    colnames(x.new) <- colnames(ozone)[-1]
    pred <- predict(ksvm.ozone, newdata=x.new, type="response")
    # prob <- attr(pred,"probabilities")[,1:2]
    if(i>=2) par(new=T)
    plot(pred~seq(min,max,length.out=9),xlab=colnames(x.new)[j],ylab="ozone",
         type="l",ylim=c(-4,4),xlim=c(-4,4),col="#0000FF33")
  }
}

windows(height=7.6, width=7); par(mfrow=c(2,2))
for (j in 5:8){
  for (i in 1:n){
    min <- x[i,j]-half
    max <- x[i,j]+half
    x.new <- data.frame(matrix(rep(x[i,],9),byrow=T,nrow=9,ncol=p))
    # x.new <- cbind(rep(x[i,1],9),rep(x[i,2],9),rep(x[i,3],9),rep(x[i,4],9))
    x.new[,j] <- seq(min,max,(max-min)/8)
    colnames(x.new) <- colnames(ozone)[-1]
    pred <- predict(ksvm.ozone, newdata=x.new, type="response")
    # prob <- attr(pred,"probabilities")[,1:2]
    if(i>=2) par(new=T)
    plot(pred~seq(min,max,length.out=9),xlab=colnames(x.new)[j],ylab="ozone",
         type="l",ylim=c(-4,4),xlim=c(-4,4),col="#0000FF33")
  }
}

# end

