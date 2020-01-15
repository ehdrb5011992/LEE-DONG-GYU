rm(list=ls())
gc()
#install.packages("lsmeans")
library(car)
library(afex)
set_sum_contrasts()

dat <- read.table('C:\\Users\\82104\\Desktop\\6_3.txt',header=T)
dat$Gender <- as.factor(dat$Gender)
repeated_variables <- as.matrix(dat[,2:ncol(dat)])
model <- lm( repeated_variables ~ Gender , data=dat)

i_data <- data.frame( "Exercise"= c(rep("A",4),rep("B",4)) , "Minute"= rep(c("10","20","30","40"),2))
result <- Anova(model , idata=i_data , idesign = ~ Exercise*Minute, type=3)

summary(result)
