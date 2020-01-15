rm(list=ls())
gc()
library(survival)
library(afex)
set_sum_contrasts()

dat <- read.csv('C:\\Users\\82104\\Desktop\\data\\7_5.csv',header=T)

# 7_5_(1)

KM <- survfit(Surv(time,censor)~1,data=dat)

par(mfrow=c(1,4))

#1. exp
LT <- KM$time
LL <- -log(KM$surv)
plot(LT,LL,type='l',xlab='t',main="exp",ylab=expression(-log(hat(S)(t))))
abline(a=0, b=lm(LL[-25] ~ LT[-25] + 0 )$coef,col='red')


#2. weibull
LT <- log(KM$time)
LL <- log(-log(KM$surv))
plot(LT,LL,type='l',xlab='log(t)',main="weibull",ylab=expression(log(-log(hat(S)(t)))))
abline(lm(LL[-25]~LT[-25])$coef,col='red')

#3. log-normal

LT <- log(KM$time)
LL <- qnorm(1-KM$surv)
plot(LT,LL,type='l',xlab='log(t)',main="log-normal",ylab=expression({Phi^-1} (1-hat(S)(t))))
abline(lm(LL[-25]~LT[-25])$coef,col='red')

#4. log-logistic

LT <- log(KM$time)
LL <- log((1-KM$surv)/KM$surv)
plot(LT,LL,type='l',xlab='log(t)',main="log-logistic",ylab=expression(log((1-hat(S)(t))/ hat(S)(t))))
abline(lm(LL[-25]~LT[-25])$coef,col='red')


# 7_5_(2)


weibull <- survreg(Surv(time,censor)~age+BMI+start_age+factor(smoke),data=dat,dist='weibull')
summary(weibull)
