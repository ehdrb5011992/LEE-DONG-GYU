# ozone model by svm

library(gclus)
data(ozone)
str(ozone)
library(e1071)
svm.ozone <- svm(Ozone ~ ., data=ozone, cost=1)
summary(svm.ozone)

windows(height=7.6, width=7)
plot(ozone$Ozone ~ svm.ozone$fitted, main="ozone study",xlim=c(0,40),ylim=c(0,40))

tune.svm <- tune(svm, Ozone ~ ., data=ozone, ranges=list(epsilon=c(0.1,1),gamma=c(0.125,0.5),cost=c(0.1,1)))
summary(tune.svm)
cor(svm.ozone$fitted,ozone$Ozone)

# another svm 
svm.ozone <- svm(Ozone ~ ., data=ozone, epsilon=1, cost=1)
summary(svm.ozone)

windows(height=7.6, width=7)
plot(ozone$Ozone ~ svm.ozone$fitted, main="ozone study",xlim=c(0,40),ylim=c(0,40))
cor(svm.ozone$fitted,ozone$Ozone)

# end