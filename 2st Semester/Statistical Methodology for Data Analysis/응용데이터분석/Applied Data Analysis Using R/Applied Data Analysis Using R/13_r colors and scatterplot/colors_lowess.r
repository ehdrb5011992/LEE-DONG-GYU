# scatterplot smoothing using lowess

require(graphics)
data(cars); str(cars)
windows(height=8,width=7.5)
plot(cars, main = "lowess(cars)",col="gray", pch=20)
lines(lowess(cars), col ="blue", lwd=1)
for (k in 1:1000) lines(lowess(cars[sample(1:50,replace=T),]), col = "blue")

windows(height=8,width=7.5)
plot(cars, main = "lowess(cars)",col="gray", pch=20)
lines(lowess(cars), col ="blue", lwd=1)
library(scales)
for (k in 1:1000) lines(lowess(cars[sample(1:50,replace=T),]), col = alpha("blue",0.05), lwd=1)

# end