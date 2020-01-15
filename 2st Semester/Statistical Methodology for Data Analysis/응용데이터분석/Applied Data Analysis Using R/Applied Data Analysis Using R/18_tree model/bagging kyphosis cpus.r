# Chapter 18. Tree model via rpart package
# bagging using ipred package
# for kyphosis
library(rpart)
data(kyphosis)
kyphosis.present <- kyphosis[kyphosis$Kyphosis=="present",]
kyphosis.absent <- kyphosis[kyphosis$Kyphosis=="absent",]
kyphosis.balanced <- rbind(kyphosis.present, kyphosis.present, kyphosis.present, kyphosis.present, kyphosis.absent)

library(ipred)
bag.kyphosis <- bagging(Kyphosis ~ Age + Number + Start, data = kyphosis.balanced, coob=T)
bag.kyphosis
addmargins(table(kyphosis.balanced$Kyphosis,predict(bag.kyphosis)))

# for cpus
data(cpus)
cpus.bag <- bagging(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data=cpus, coob=T, nbagg=100)
cpus.bag

# end



