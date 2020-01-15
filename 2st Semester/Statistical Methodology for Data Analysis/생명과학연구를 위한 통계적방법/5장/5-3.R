rm(list=ls())
gc()
#install.packages("lsmeans")
library(lsmeans)
library(dplyr)

dat<-read.table('C:\\Users\\82104\\Desktop\\5_3.txt',header=T)

full <- lm(y~trt*x,data=dat)
m1 <-  car::Anova(full,type=3)
m1

restricted <- lm (y~trt + x,data=dat)
m2 <- car::Anova(restricted,type=3)
m2

aov(y~trt+x , data=dat) %>% lsmeans("trt")

