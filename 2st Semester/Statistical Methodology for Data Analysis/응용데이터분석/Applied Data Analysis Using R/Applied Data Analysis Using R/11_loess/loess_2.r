# Chapter 11. loess of Sulfate Deposits

Sulfate <- read.table("Sulfate.txt", header=T)
library(lattice)
cloud(SO4 ~ Longitude * Latitude, data=Sulfate, aspect=c(1,0.5), screen =list(z=30,x=-60))
x11(); cloud(SO4 ~ Longitude * Latitude, data=Sulfate, aspect=c(1,0.5), screen =list(z=30,x=-30))

Sulfate.loess <- loess(SO4 ~ Longitude + Latitude, data=Sulfate, family="symmetric", normalize=F)
attach(Sulfate)
grid.data <- expand.grid(Longitude = seq(68,124,1), Latitude = seq(25,49,1))
Sulfate.z <- predict(Sulfate.loess, newdata = grid.data)

wireframe(Sulfate.z ~ Longitude*Latitude, data=grid.data, aspect=c(1,0.5), screen =list(z=30,x=-60))
x11(); wireframe(Sulfate.z ~ Longitude*Latitude, data=grid.data, aspect=c(1,0.5), screen =list(z=30,x=-30))

contourplot(Sulfate.z ~ Longitude * Latitude, grid.data)

hist(Sulfate.loess$residuals, nclass=16)
x11(); boxplot(Sulfate.loess$residuals, xlim=c(-1,2), horizontal=T, xlab="Residuals")

# end