# Correspondence analysis for 2007 Presidential Election Survey data
F0 <- read.table("Election.txt", header=T)
F <- as.matrix(F0[,-1])
rownames(F) <- F0[,1]
addmargins(F)
r.margin <- addmargins(F)[,7]
c.margin <- addmargins(F)[8,]

# MASS corresp() 
library(MASS)
corresp.F <- corresp(F,nf=2)
attach(corresp.F)
ROW <- rscore%*%diag(cor)
COL <- cscore

plot(ROW[,2]~ROW[,1],type="n",xlim=c(-2,2),ylim=c(-2,2),xlab="First Dimension",ylab="Second Dimension",main="Presidential Election 2007")
text(ROW[,1],ROW[,2],labels=rownames(ROW),cex=1,col="blue")
par(new=T)
plot(COL[,2]~COL[,1],type="n",xlim=c(-2,2),ylim=c(-2,2),xlab="",ylab="")
text(COL[,1],COL[,2],labels=rownames(COL),cex=1,col="red")
x11()
plot(ROW[,2]~ROW[,1],type="n",xlim=c(-2,2),ylim=c(-2,2),xlab="First Dimension",ylab="Second Dimension",main="Presidential Election 2007")
text(ROW[,1],ROW[,2],labels=rownames(ROW),cex=r.margin/sum(r.margin)*12,col="blue")
par(new=T)
plot(COL[,2]~COL[,1],type="n",xlim=c(-2,2),ylim=c(-2,2),xlab="",ylab="")
text(COL[,1],COL[,2],labels=rownames(COL),cex=c.margin/sum(c.margin)*12,col="red")

# Symmetric Plot
x11(); par(mfrow=c(1,1))
plot(rscore[,2]~rscore[,1],type="n",xlim=c(-3.5,3.5),ylim=c(-3.5,3.5),xlab="First Dimension",ylab="Second Dimension",main="Presidential Election 2007")
text(rscore[,1],rscore[,2],labels=rownames(rscore),cex=0.8,col="blue")
par(new=T)
plot(cscore[,2]~cscore[,1],type="n",xlim=c(-3.5,3.5),ylim=c(-3.5,3.5),xlab="",ylab="")
text(cscore[,1],cscore[,2],labels=rownames(cscore),cex=0.8,col="red")

# Alternative Biplot
biplot(rscore,cscore,cex=1,xlab="First Dimension",ylab="Second Dimension",main="Presidential Election 2007")
# end