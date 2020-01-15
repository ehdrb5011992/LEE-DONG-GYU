rm(list=ls())
#install.packages("nnet")
#install.packages("lmtest")
#install.packages("afex")
library(nnet)
library(lmtest)
library(afex)
dat<-read.table('C:\\Users\\82104\\Desktop\\4_3.txt',header=T)
set_sum_contrasts()


dat$Foodvar <- as.character(dat$Foodvar); dat$Length <- as.character(dat$Length)
dat$Foodvar[dat$Foodvar == 'E'] <- '1E' #기준변수를 바꾸기 위한 작업. 기타(E)에 해당하는 변수를 기준으로 하고싶다. 편법.
dat$Length[dat$Length == 2] <- 0 #기준변수를 바꾸기 위한 작업. 길이>2.3 (2)에 해당하는 변수를 기준으로 하고 싶다. 편법.
dat$Foodvar <- as.factor(dat$Foodvar) ; dat$Length <- as.factor(dat$Length)

gl <- multinom(Foodvar ~ Length+Lake, weights=count , data=dat )
t(summary(gl)$coefficients)[c(1,4,5,3,2),c(3,4,1,2)]
satu <- multinom(Foodvar ~ Length*Lake , weights=count , data=dat)

lrtest(gl,satu) #적합도 검정.






#################################################################################################################################
#Q. Lake, Length , Intercept는?
leng <- multinom(Foodvar ~ Length, weights=count , data=dat)
leng2 <- multinom(Foodvar ~ 1, weights=count , data=dat)


# mat <- as.data.frame(model.matrix(~Length+Lake, data=dat))
# mat["Foodvar"] <- dat$Foodvar
# mat["count"] <- dat$count
# gl <- multinom(Foodvar ~ .+0-count, weights=count , data=mat )
# lrtest(gl,c("Lake1","Lake2","Lake3","Length1"))





###########
data(iris)
str(iris)
mat <- as.data.frame(model.matrix(~Species+Petal.Length+Sepal.Length, data=iris))
mm <- multinom(Sepal.Length~.+0, data=mat, trace=F)

?model.matrix
lrtest(mm, "Speciesversicolor")

for (var in mm$coefnames[-1]) {
  print(paste(var, "--", lrtest(mm, var)[[5]][2]))
}

###########
options(contrast="contr.sum")
mat <- as.data.frame(model.matrix(~Length+Lake, data=dat))
mat["Foodvar"] <- dat$Foodvar
mat["count"] <- dat$count
gl <- multinom(Foodvar ~ .+0-count, weights=count , data=mat )
lrtest(gl,c("LakeH","LakeO","LakeT","Length1"))
#################

gl <- multinom(Foodvar ~ Length+Lake, weights=count , data=dat )
lrtest(gl,c("LakeH","LakeO","LakeT","Length1"))
t(summary(gl)$coefficients)[c(1,4,5,3,2),c(3,4,1,2)]

gl

?lrtest














#예측 솔루션
pred <- gl$fitted.values 
pred <- pred[seq(1,40,5),]
rownames(pred) <- c("(H,1)","(H,2)","(O,1)","(O,2)","(T,1)","(T,2)","(G,1)","(G,2)")
colnames(pred) <- c("E","A","B","F","M")
pred<-pred[,c(4,5,2,3,1)]
pred




z <-  t( summary(gl)$coefficients/summary(gl)$standard.errors )
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

