# multidimensional scaling

vote <- read.csv("house-votes-84-number coded.txt", header=F)
rownames(vote) <- 1:435  
colnames(vote) <- c("party","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10","v11","v12","v13","v14","v15","v16")
str(vote)  # 435 observations

D <- dist(vote[,2:17], method="manhattan")
as.matrix(D)[1:5,1:5]
rbind(vote[1,2:17],vote[2,2:17])

mds.classic <- cmdscale(D)
plot(mds.classic[,1],mds.classic[,2], type="n", main="US Congress Vote 1984", xlab="First Dim", ylab="Second Dim", xlim=c(-20,20), ylim=c(-17,17))
text(mds.classic[,1],mds.classic[,2], 1:435, col=c("blue","red")[unclass(vote$party)],cex=0.7)

mds.classic <- cmdscale(D, k=3, eig=T)
plot(mds.classic$eig[1:10],xlab="dim")
pairs(mds.classic$points[,1:3],labels=c("dim 1","dim 2","dim 3"),col=c("blue","red")[unclass(vote$party)])

# nonmetric MDS
library(MASS)
library(data.table)
vote.1 <- unique(vote)
str(vote.1)  # 342 observations
D.1 <- dist(vote.1)

mds.isoMDS <- isoMDS(D.1)
x11()
plot(mds.isoMDS$points[,1],mds.isoMDS$points[,2], type="n", main="US Congress Vote 1984", xlab="First Dim", ylab="Second Dim", xlim=c(-5,5), ylim=c(-4,4))
text(mds.isoMDS$points[,1],mds.isoMDS$points[,2], rownames(vote.1), col=c("blue","red")[unclass(vote.1$party)],cex=0.7)

isoMDS(D.1, k=2)$stress
isoMDS(D.1, k=3)$stress
isoMDS(D.1, k=4)$stress
isoMDS(D.1, k=5)$stress

mds.isoMDS <- isoMDS(D.1, k=3)
pairs(mds.isoMDS$points[,1:3],labels=c("dim 1","dim 2","dim 3"),col=c("blue","red")[unclass(vote.1$party)]) 

library(vegan)
mds.vegan <- monoMDS(D)
x11()
plot(mds.vegan$points[,1],mds.vegan$points[,2], type="n", main="US Congress Vote 1984", xlab="First Dim", ylab="Second Dim", xlim=c(-1.5,1.5), ylim=c(-1.2,1.2))
text(mds.vegan$points[,1],mds.vegan$points[,2], rownames(vote), col=c("blue","red")[unclass(vote$party)],cex=0.7)

# end

plot(mds.isoMDS$points[,1],mds.isoMDS$points[,2], type="n", main="", xlab="", ylab="", xlim=c(-5,5), ylim=c(-4,4), axes=F)
text(mds.isoMDS$points[,1],mds.isoMDS$points[,2], rownames(vote.1), col=c("blue","red")[unclass(vote.1$party)],cex=0.7)
savePlot(type="jpg")

# end

