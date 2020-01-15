# Chapter 15. Graphs 2
# 2-dim density
data(geyser, package="MASS")
str(geyser)
y <- geyser$duration
x <- geyser$waiting

n <- 10000
index <- sample(1:299, n, replace=T)
y <- geyser$duration[index]+rnorm(n,0,0.1)
x <- geyser$waiting[index]+rnorm(n,0,1)
windows(height=7.5, width=7)
plot(x, y, xlab="waiting time",ylab="duration time", main="Yellowstone Geyser")

library(gplots)
hist2d(x,y,nbins=50,xlab="waiting time",ylab="duration time",main="Yellowstone Geyser")

library(hexbin)
hexbinplot(y~x, colorkey=F, aspect=1, xlab="waiting time",ylab="duration time",main="Yellowstone Geyser")

library(gplots); ci2d(x,y, xlab="duration", ylab="waiting")

# end