# Chapter 18. Tree model
# random forest model

library(rpart)
data(kyphosis); str(kyphosis)
kyphosis.present <- kyphosis[kyphosis$Kyphosis=="present",]
kyphosis.absent <- kyphosis[kyphosis$Kyphosis=="absent",]
kyphosis.balanced <- rbind(kyphosis.present, kyphosis.present, kyphosis.present, kyphosis.present, kyphosis.absent)

library(randomForest)
RF.1 <- randomForest(Kyphosis ~ Age+Number+Start, data=kyphosis.balanced, var.importance=T)
RF.1$importance

# for cpus data
library(rpart)
data(cpus, package="MASS"); str(cpus)
RF.2 <- randomForest(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data=cpus,importance=TRUE)
RF.2
varImpPlot(RF.2)

# end