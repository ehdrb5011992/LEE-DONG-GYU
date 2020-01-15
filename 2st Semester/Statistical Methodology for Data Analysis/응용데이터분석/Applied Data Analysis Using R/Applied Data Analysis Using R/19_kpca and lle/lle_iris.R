# Chapter 19. Locally linear embedding for the Iris Data

library(lle)
data(iris)
X <- scale(iris[,1:4])
lle.iris <- lle(X, m=2, k=25)

windows(height=8, width=7)
plot(lle.iris$Y, col=c("red","green","blue")[unclass(iris[,5])], main="lle of iris",
     xlim=c(-5,5),ylim=c(-5,5),xlab="1st dimension",ylab="2nd dimension")

par(mfrow=c(2,2))
plot(lle.iris$Y, col=topo.colors(150)[rank(iris[,1])], main="sepal length",
     xlim=c(-5,5),ylim=c(-5,5),xlab="1st dimension",ylab="2nd dimension")
plot(lle.iris$Y, col=topo.colors(150)[rank(iris[,2])], main="sepal width",
     xlim=c(-5,5),ylim=c(-5,5),xlab="1st dimension",ylab="2nd dimension")
plot(lle.iris$Y, col=topo.colors(150)[rank(iris[,3])], main="petal length",
     xlim=c(-5,5),ylim=c(-5,5),xlab="1st dimension",ylab="2nd dimension")
plot(lle.iris$Y, col=topo.colors(150)[rank(iris[,4])], main="petal width",
     xlim=c(-5,5),ylim=c(-5,5),xlab="1st dimension",ylab="2nd dimension")

# end

