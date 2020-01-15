rm(list=ls())
gc()
library(KMsurv)
library(reshape)

dat <- read.csv('C:\\Users\\82104\\Desktop\\data\\7_2.csv',header=T)
dat <- cast(dat,time~censor)
colnames(dat) <- c("time","censor","death")
dat$time <- floor(dat$time)

#life table
life_table <- with(dat,lifetab(c(time,NA),sum(c(censor,death)),censor,death))
life_table


#plot
par(mfrow=c(1,2))
length(time)
with(dat,plot(time,life_table[,5],type='s',xlab='Year',ylab='Survival',ylim=c(0,1)))
with(dat,plot(time,life_table[,5],type='o',xlab='Year',ylab='Survival',ylim=c(0,1)))

dev.off()