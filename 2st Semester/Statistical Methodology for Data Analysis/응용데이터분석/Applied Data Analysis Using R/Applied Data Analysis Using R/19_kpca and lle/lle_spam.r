# Chapter 19. Locally linear embedding for the spam data

library(lle)
library(kernlab)
data(spam); str(spam)

z <- data.frame(scale(spam[,-58]))
var.name <- colnames(spam[,-58])
n <- nrow(z); n
p <- ncol(z); p

lle.spam <- lle(z,m=2, k=100)

windows(height=8,width=7) 
plot(lle.spam$Y, col=c("#0000FF22","#FF000055")[spam[,58]], main="lle of spam",
     xlim=c(-5,5),ylim=c(-5,5),xlab="1st dimension",ylab="2nd dimension")

windows(height=5,width=8) 
par(mfrow=c(1,2))
plot(lle.spam$Y,pch=21,cex=0.4,col=topo.colors(n,alpha=0.5)[rank(spam$make)],xlim=c(-5,5),ylim=c(-5,5),xlab="first dim",ylab="second dim",main="make")
plot(lle.spam$Y,pch=21,cex=0.4,col=topo.colors(n,alpha=0.5)[rank(spam$address)],xlim=c(-5,5),ylim=c(-5,5),xlab="first dim",ylab="second dim",main="address")

windows(height=5,width=8) 
par(mfrow=c(1,2))
plot(lle.spam$Y,pch=21,cex=0.4,col=topo.colors(n,alpha=0.5)[rank(spam$you)],xlim=c(-5,5),ylim=c(-5,5),xlab="first dim",ylab="second dim",main="you")
plot(lle.spam$Y,pch=21,cex=0.4,col=topo.colors(n,alpha=0.5)[rank(spam$charExclamation)],xlim=c(-5,5),ylim=c(-5,5),xlab="first dim",ylab="second dim",main="charExclamation")

windows(height=5,width=8) 
par(mfrow=c(1,2))
for (j in 1:57) {
    plot(lle.spam$Y,pch=21,cex=0.4,col=topo.colors(n,alpha=0.5)[rank(spam[,j])],xlim=c(-5,5),ylim=c(-5,5),xlab="first dim",ylab="second dim",main=paste(var.name[j]))
    Sys.sleep(1)
}

# end
