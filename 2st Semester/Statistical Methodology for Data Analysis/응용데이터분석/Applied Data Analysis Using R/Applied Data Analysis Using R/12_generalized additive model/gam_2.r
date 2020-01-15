# gam of equipment

equip <- read.table("equip.txt", header=T)
str(equip)
attach(equip)
plot(removal, type="l", lty="dotted")
text(x=1:48, y=removal, label=month)

library(mgcv)
equip.m <- gam(removal ~ year+s(month,k=5), data=equip, family=poisson)
summary(equip.m)
plot(equip.m, ylim=c(-1,1), ylab="Month Effect")

# end