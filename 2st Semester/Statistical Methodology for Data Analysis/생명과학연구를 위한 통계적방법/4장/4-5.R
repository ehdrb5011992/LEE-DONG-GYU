rm(list=ls())
#install.packages("ordinal")
library(lmtest)
library(afex)
library(MASS)
library(nnet)
library(ordinal)

set_sum_contrasts()

dat<-read.table('C:\\Users\\82104\\Desktop\\4_5.txt',header=T)
dat$Social <- as.factor(dat$Social)
dat$MentalDis <- as.factor(dat$MentalDis)
ordered_logistic <- polr(MentalDis~Social+Case,data=dat) #SAS와 조금 다른 함수계산.
#ordered_logistic <- clm(MentalDis~Social+Case,data=dat)
alter <- multinom(MentalDis ~ Social+Case ,  data=dat)
lrtest(ordered_logistic,alter) #비례오즈 적합도검정.
summary(ordered_logistic)
head(round(ordered_logistic$fitted.values,5)) #예측값.

coef <- ordered_logistic$coefficients
odds_ratio <- exp(-1*coef*c(2,1)) #SAS와 같이 보기좋게 하려고 -1배를 해줌. case는 범주형이아님.
print(odds_ratio)

########################################################## 아래는 고민중.
anova(ordered_logistic,type="III")
drop1(ordered_logistic,test="Chi")

t1<-glm(MentalDis~Social+Case,data=dat,family="binomial")
t2<-glm(MentalDis~1,data=dat,family="binomial")
lrtest(t1,t2)

propotional_logits <- multinom(MentalDis ~ Social+Case,data=dat)
satu <- multinom(MentalDis ~ Social*Case ,  data=dat)
lrtest(propotional_logits,satu) #적합도 검정.


fit.prop = polr(diag~age,weights=count,data=data)
summary(fit.prop)
fit.dev = multinom(diag ~ age, weights = count, data = data)
deviance(fit.prop)-deviance(fit.dev)
1 - pchisq(deviance(fit.prop)-deviance(fit.dev), df = 2)

exp(cbind(OR = coef(fit.prop)[1:2], CI95lo = confint(fit.prop)[1:2, 1], CI95hi = confint(fit.prop)[1:2, 2]))
