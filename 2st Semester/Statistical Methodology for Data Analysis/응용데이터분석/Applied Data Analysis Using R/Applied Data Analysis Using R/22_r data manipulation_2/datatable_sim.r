# searching using data.table package

library(data.table)
n <- 10000000
digits <- as.factor(0:9)
x1 <- sample(digits, n, replace=T)
x2 <- sample(digits, n, replace=T)
x3 <- sample(digits, n, replace=T)
x4 <- sample(digits, n, replace=T)
x5 <- sample(digits, n, replace=T)
x6 <- sample(digits, n, replace=T)

DT <- data.table(x1, x2, x3, x4, x5, x6, y=rnorm(n))
head(DT, 10)
class(DT)

setkey(DT, x1, x2, x3, x4, x5, x6)
head(DT, 10)

DT[J("0","0","0","0","0","0")]

p.time <- proc.time()
DT[J("1","2","3","4","5","6")]
proc.time() - p.time

p.time <- proc.time()
DT[x1=="1" & x2=="2" & x3=="3" & x4=="4" & x5=="5" & x6=="6",]
proc.time() - p.time

# end

p.time <- proc.time()  # using data.table for data screening
DT <- data.table(x1, x2, x3, x4, x5, x6, y=rnorm(n))
setkey(DT, x1, x2, x3, x4, x5, x6)
DT[J("1","2","3","4","5","6")]
proc.time() - p.time

p.time <- proc.time()  # using data.frame for data screening
DF <- data.frame(x1, x2, x3, x4, x5, x6, y=rnorm(n))
DF[x1=="1" & x2=="2" & x3=="3" & x4=="4" & x5=="5" & x6=="6",]
proc.time() - p.time

# end

