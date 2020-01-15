# visualizing the data with three variables

library(mclust)
data(diabetes)
str(diabetes)
attach(diabetes)
pairs(diabetes[,2:4], col=c("blue","red","green3")[unclass(class)], size=3)

library(scatterplot3d)
scatterplot3d(glucose, insulin, sspg, color=c("blue","red","green3")[unclass(class)], angle=30, pch=20)

library(rgl)
plot3d(glucose, insulin, sspg, col=c("blue","red","green3")[unclass(class)], size=3)

library(lattice)
insulin.grp <- equal.count(insulin, number=4, overlap=0)
xyplot(sspg ~ glucose | insulin.grp)
xyplot(sspg ~ glucose | class)

# end
