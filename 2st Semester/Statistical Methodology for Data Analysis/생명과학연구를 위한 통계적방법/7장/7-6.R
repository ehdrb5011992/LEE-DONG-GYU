rm(list=ls())
gc()
library(survival)
library(My.stepwise)


dat <- read.csv('C:\\Users\\82104\\Desktop\\data\\7_6.csv',header=T)
cox <- coxph(Surv(time,censor)~factor(age)+factor(Indx),data=dat,ties='breslow')
summary(cox)

My.stepwise.coxph(Time="time",Status="censor",variable.list=c("age","Indx"),data=dat) #do not breslow



