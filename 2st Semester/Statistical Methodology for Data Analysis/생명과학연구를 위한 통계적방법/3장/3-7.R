rm(list=ls())
x <-matrix(c(794,150,86,570),byrow=T,2)
dimnames(x) = list(c("o","x"),c('o','x'))
x

mcnemar.test(x,correct=F)

