# chapter 20. visualization of SVM for the iris data (Versicolor vs Virginica).

library(e1071)
data(iris); attach(iris)
x <- scale(iris[51:150,1:4])
y <- iris[51:150,5]
levels(y) <- list(versicolor="versicolor",virginica="virginica")
iris.svm <- svm(x, y, probability=TRUE)
summary(iris.svm)
table(iris.svm$fitted, y)

n <- nrow(x)
half <- 0.5
par(mfrow=c(2,2))
for (j in 1:4){
  for (i in 1:n){
    min <- x[i,j]-half
    max <- x[i,j]+half
    x.new <- cbind(rep(x[i,1],9),rep(x[i,2],9),rep(x[i,3],9),rep(x[i,4],9))
    x.new[,j] <- seq(min,max,(max-min)/8)
    colnames(x.new) <- colnames(iris)[1:4]
    pred <- predict(iris.svm,newdata=x.new,probability=TRUE)
    prob <- attr(pred,"probabilities")[,1:2]
    if(i>=2) par(new=T)
    plot(prob[,1]~seq(min,max,length.out=9),xlab=colnames(x.new)[j],ylab="versicolor",
         type="l",ylim=c(0,1),xlim=c(-4,4),col="#0000FF33")
  }
}

# end

