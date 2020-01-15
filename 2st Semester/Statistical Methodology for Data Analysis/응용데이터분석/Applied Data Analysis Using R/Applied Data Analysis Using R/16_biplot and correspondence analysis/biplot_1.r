# r biplot for protein data

X.1 <- read.table("protein.txt",header=T)
str(X.1)
X <- X.1[,2:10]
n <- nrow(X)

# Using R function princomp()
pca.X <- princomp(X, cor=T)
pca.X$scores <- pca.X$scores*sqrt((n-1)/n)
# round(pca.X$scores,3)
# round(apply(pca.X$scores,2,sd),3)
biplot(pca.X,scale=0,cex=0.8,xlab="First Dimension",ylab="Second Dimension")
X.1$Country
summary(pca.X,loadings=T)

# end
