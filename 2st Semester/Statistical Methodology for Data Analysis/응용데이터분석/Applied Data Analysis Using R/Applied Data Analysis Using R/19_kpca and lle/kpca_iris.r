# Chapter 19. Kernel pca for the iris 
library(kernlab)
data(iris)
kpca.iris <- kpca(~., data=data.frame(scale(iris[,-5])), kernel="rbfdot", kpar=list(sigma=0.2), features=2)

windows(height=8, width=7.2)
plot(rotated(kpca.iris),col= as.integer(iris[,5]),xlab="1st dimension",ylab="2nd dimension",
     main="Iris (sigma=0.2)", xlim=c(-10,10), ylim=c(-10,10))

windows(height=8, width=7.2)
par(mfrow=c(2,2))
plot(rotated(kpca.iris),col=topo.colors(150)[rank(iris[,1])],xlab="1st dimension",ylab="2nd dimension",
     main="sepal length", xlim=c(-10,10), ylim=c(-10,10) )
plot(rotated(kpca.iris),col=topo.colors(150)[rank(iris[,2])],xlab="1st dimension",ylab="2nd dimension",
     main="sepal width", xlim=c(-10,10), ylim=c(-10,10) )
plot(rotated(kpca.iris),col=topo.colors(150)[rank(iris[,3])],xlab="1st dimension",ylab="2nd dimension",
     main="petal length", xlim=c(-10,10), ylim=c(-10,10) )
plot(rotated(kpca.iris),col=topo.colors(150)[rank(iris[,4])],xlab="1st dimension",ylab="2nd dimension",
     main="petal width", xlim=c(-10,10), ylim=c(-10,10) )

# end

