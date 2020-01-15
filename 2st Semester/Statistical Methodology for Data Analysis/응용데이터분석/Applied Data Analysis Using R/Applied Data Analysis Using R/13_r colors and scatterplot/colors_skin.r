# UCI Machine Learning Repository Skin_NonSkin data

skin <- read.table("Skin_NonSkin.txt")
colnames(skin) <- c("R","G","B","class")
str(skin)
table(skin$class)
n <- nrow(skin)
attach(skin)
windows(height=5.5, width=10)
par(mfrow=c(1,2))
plot(R, G, col=c("red3","green3")[class],pch=20, main="Skin vs NonSkin")
plot(R[n:1], G[n:1], col=c("red3","green3")[class[n:1]],pch=20, main="Skin vs NonSkin")

sub <- sample(1:n, 3200, replace=F); x11()
plot(R[sub], G[sub], col=c("red3","green3")[class[sub]],pch=20, main="Skin vs NonSkin")

library(scales)
color <- c(alpha("red3", 0.05),alpha("green3", 0.05))
sub <- sample(1:n); windows(height=8,width=7.5)
plot(R[sub], G[sub], col=color[class[sub]],pch=20, main="Skin vs NonSkin")

windows(height=8,width=7.5); plot(R[sub], B[sub], col=color[class[sub]],pch=20, main="Skin vs NonSkin")
windows(height=8,width=7.5); plot(G[sub], B[sub], col=color[class[sub]],pch=20, main="Skin vs NonSkin")

# end

