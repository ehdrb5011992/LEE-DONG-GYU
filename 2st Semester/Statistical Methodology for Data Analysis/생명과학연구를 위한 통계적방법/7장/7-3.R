rm(list=ls())
gc()
library(survival)

dat <- read.csv('C:\\Users\\82104\\Desktop\\data\\7_3.csv',header=T)

KM <- survfit(Surv(time,censor)~1 , conf.type='log-log',data=dat)
summary(KM)

quantile(KM,probs=c(0.25,0.5,0.75),conf.int=T)

plot(KM,xlab='Day',ylab='Survival',mark.time=T)
legend(0.3,0.3,c("KM","95% CI"),lty=c(1,2))

