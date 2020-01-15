rm(list=ls())
gc()
library(lmtest)
library(tidyr)

dat<-read.table('C:\\Users\\82104\\Desktop\\4_7.txt',header=T)
dat <- as_tibble(dat)
data <- dat %>% spread(key=Death,value=count)
colnames(data) <- c("x","y","z_N","z_Y")
deviance(logistic)

logistic$
data <- data.frame(belt, car, death.Y, death.N)
logistic <- glm(cbind(data$z_Y,data$z_N) ~ x + y, family="binomial", data=data)
summary(logistic)
exp(cbind( Odds_ratio = logistic$coefficients[2:3]*2 , 
           ci_lower_95 = confint(logistic)[2:3,1]*2 , ci_upper_95 = confint(logistic)[2:3,2]*2 ))


