# Chapter 19. Kernel pca for the spam data

library(kernlab)
data(spam); str(spam)

z <- data.frame(scale(spam[,-58]))
var.name <- colnames(spam[,-58])
n <- nrow(z); n
p <- ncol(z); p

k.princomp <- kpca(~., data=z,kernel="rbfdot",kpar=list(sigma=0.01),features=2)
kpc.scores <- rotated(k.princomp)

windows(height=8,width=7.2)
max.lim <- max(abs(kpc.scores))*1.1
color <- c("#0000FF55","#FF000055")[unclass(spam[,58])]
plot(kpc.scores,pch=21,cex=0.4,col=color,xlim=c(-max.lim,max.lim),ylim=c(-max.lim,max.lim),xlab="first dim",ylab="second dim",main="Kernel PCA of Spam Data")

windows(height=5,width=8)
par(mfrow=c(1,2))
plot(kpc.scores,pch=21,cex=0.4,col=topo.colors(n,alpha=0.5)[rank(spam$make)],xlim=c(-max.lim,max.lim),ylim=c(-max.lim,max.lim),xlab="first dim",ylab="second dim",main="make")
plot(kpc.scores,pch=21,cex=0.4,col=topo.colors(n,alpha=0.5)[rank(spam$address)],xlim=c(-max.lim,max.lim),ylim=c(-max.lim,max.lim),xlab="first dim",ylab="second dim",main="address")

windows(height=5,width=8)
par(mfrow=c(1,2))
plot(kpc.scores,pch=21,cex=0.4,col=topo.colors(n,alpha=0.5)[rank(spam$your)],xlim=c(-max.lim,max.lim),ylim=c(-max.lim,max.lim),xlab="first dim",ylab="second dim",main="your")
plot(kpc.scores,pch=21,cex=0.4,col=topo.colors(n,alpha=0.5)[rank(spam$you)],xlim=c(-max.lim,max.lim),ylim=c(-max.lim,max.lim),xlab="first dim",ylab="second dim",main="you")

windows(height=9,width=8)
par(mfrow=c(2,2))
for (j in 1:57) {
    plot(kpc.scores,pch=21,cex=0.4,col=topo.colors(n,alpha=0.5)[rank(spam[,j])],xlim=c(-max.lim,max.lim),ylim=c(-max.lim,max.lim),xlab="first dim",ylab="second dim",main=paste(var.name[j]))
    Sys.sleep(1)
}

# end
