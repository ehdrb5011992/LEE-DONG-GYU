# gam of kyphosis

kyphosis <- read.table("kyphosis.txt", header=T)
str(kyphosis)
attach(kyphosis)
library(mgcv)

kypho.m <- gam(Kypho ~ s(Age)+s(StartVert)+s(NumVert), family=binomial)
summary(kypho.m)
par(mfrow=c(1,3))
plot(kypho.m)  

# end
