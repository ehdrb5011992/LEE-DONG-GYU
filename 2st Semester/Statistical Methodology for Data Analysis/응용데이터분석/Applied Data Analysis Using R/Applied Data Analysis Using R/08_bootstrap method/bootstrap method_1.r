# bootstrapping a single statistic: bivariate correlation 

library(boot)
x <- c(15,26,10,9,15,20,18,11,8,20,7,9,10,11,11,10,12,17,11,10)
y <- c(95,71,83,91,102,87,93,100,104,94,113,96,83,84,102,100,105,121,86,100)
cor.X <- function(data, indices) cor(data[indices,1],data[indices,2])

boot.log <- boot(data=cbind(x,y), statistic=cor.X, R=1000)
print(boot.log)

hist(boot.log$t, xlim=c(-1,1), nclass=20, main="bootstrap correlations", xlab="corr")
boot.ci(boot.log, type = c("perc", "bca"))

# end