rm(list=ls())

x <- array(c(2,0,21,10,
             2,0,40,18,
             6,0,33,10,
             17,0,16,4) ,dim=c(2,2,4) , dimnames=list(sex=c("m","f"),state=c('x','o'),severity=c(1,2,3,4)))
x
mantelhaen.test(x,correct=F)
