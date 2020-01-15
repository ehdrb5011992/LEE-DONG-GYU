# hexbin plots with skin data

skin <- read.table("Skin_NonSkin.txt")
colnames(skin) <- c("R","G","B","class")
str(skin)
table(skin$class)
attach(skin)

library(hexbin)
hexbinplot(R ~ G)
R1 <- R[class==1]; G1 <- G[class==1]
R2 <- R[class==2]; G2 <- G[class==2]

hexbinplot(G1 ~ R1, xlim=c(0,256), ylim=c(0,256), colorkey=F, main="Skin Cases")
x11(); hexbinplot(G2 ~ R2, xlim=c(0,256), ylim=c(0,256), colorkey=F, main="NonSkin Cases")

x11(); hexbinplot(G1 ~ R1, xlim=c(0,256), ylim=c(0,256), colorkey=F, main="Skin Cases", colramp = function(n) terrain.colors(n))
x11(); hexbinplot(G2 ~ R2, xlim=c(0,256), ylim=c(0,256), colorkey=F, main="NonSkin Cases", colramp = function(n) terrain.colors(n))

# end