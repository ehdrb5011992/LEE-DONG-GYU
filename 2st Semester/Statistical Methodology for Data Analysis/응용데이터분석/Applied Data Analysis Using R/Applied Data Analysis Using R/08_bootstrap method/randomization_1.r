# randomization test of the independence

library(coin)
independence_test(asat ~ group, data = asat)

x <- c(15,26,10,9,15,20,18,11,8,20,7,9,10,11,11,10,12,17,11,10)
y <- c(95,71,83,91,102,87,93,100,104,94,113,96,83,84,102,100,105,121,86,100)
r <- cor(x,y)
r
independence_test(y ~ x, alternative="less")

# end