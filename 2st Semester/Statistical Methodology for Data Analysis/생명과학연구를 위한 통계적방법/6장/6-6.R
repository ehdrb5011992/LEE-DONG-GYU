rm(list=ls())
gc()
#install.packages("lsmeans")
library(car)
library(afex)
set_sum_contrasts()

dat <- read.table('C:\\Users\\82104\\Desktop\\6_6.txt',header=T)





dat$medicine <- as.factor(dat$medicine)
repeated_variables <- as.matrix(dat[,1:ncol(dat)-1])
model <- lm( repeated_variables ~ medicine , data=dat)

i_data <- data.frame( "Month"= c("0","1","2","3","4"))
result <- Anova(model , idata=i_data , idesign = ~ Month, type=3)

summary(result)
