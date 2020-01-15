rm(list=ls())
gc()
#install.packages("lsmeans")
library(lsmeans)
library(dplyr)
library(afex)
set_sum_contrasts()

dat<-read.table('C:\\Users\\82104\\Desktop\\5_7.txt',header=T)
dat$type <- as.factor(dat$type)

full <- lm(y~.^3 -type:x1:x2 -x1:x2 -1,data=dat)

m1 <-  car::Anova(full,type=3)
m1

