# multidimensional scaling, #2

vote <- read.csv("house-votes-84-number coded.txt", header=F)
rownames(vote) <- 1:435  
colnames(vote) <- c("party","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10","v11","v12","v13","v14","v15","v16")
str(vote)  # 435 observations
D <- dist(vote[,2:17], method="manhattan")

library(vegan)
i.map <- isomap(D, k=5)
plot(i.map, main="US Congress Vote 1984")
x11()
plot(i.map$points[,1], i.map$points[,2], type="n", main="US Congress Vote 1984", xlab="First Dim", ylab="Second Dim", xlim=c(-20,20), ylim=c(-17,17))
text(i.map$points[,1], i.map$points[,2], 1:435, col=c("blue","red")[unclass(vote$party)],cex=0.8)

plot(i.map$eig[1:10], xlab="dim")
pairs(i.map$points[,1:3],labels=c("dim 1","dim 2","dim 3"),col=c("blue","red")[unclass(vote$party)])

# end