# SVM of the spam data

library(e1071)
library(kernlab)
data(spam); str(spam)
svm.model <- svm(type ~ ., data=spam, gamma=1, cost=1) 
summary(svm.model) 
addmargins(table(spam$type, svm.model$fitted))

n <- nrow(spam)
sub <- sample(1:n, round(0.75*n))
spam.1 <- spam[sub,]
spam.2 <- spam[-sub,]

svm.model.1 <- svm(type ~ ., data=spam.1, gamma=1, cost=1)
summary(svm.model.1) 
addmargins(table(spam.1$type, svm.model.1$fitted))

svm.predict.2 <- predict(svm.model.1, newdata=spam.2)
addmargins(table(spam.2$type, svm.predict.2))

# tuning
p.time <- proc.time()
tune.svm <- tune(svm, type ~ .,  data=spam.1, ranges=list(gamma=c(0.1,1,10),cost=c(0.1,1,10)))
summary(tune.svm)  # best parameters are 0.1 and 10
proc.time()-p.time

# class weights
class.wts <- (n/2)/table(spam$type)
svm.model <- svm(type ~ ., data=spam, gamma=0.1, cost=10, class.weights=class.wts) 
summary(svm.model) 
addmargins(table(spam$type, svm.model$fitted))

p.time <- proc.time()
tune.svm <- tune(svm, type ~ .,  data=spam.1, ranges=list(gamma=c(0.1,1,10),cost=c(0.1,1,10)), class.weights=class.wts)
summary(tune.svm)  # best parameters are 0.1 and 10
proc.time()-p.time

# end