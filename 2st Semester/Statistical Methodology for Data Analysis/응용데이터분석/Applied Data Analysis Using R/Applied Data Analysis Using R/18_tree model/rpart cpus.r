# Chapter 18. Tree model via rpart package
# bagging for cpus data

library(rpart)
data(cpus, package="MASS"); str(cpus)
hist(cpus$perf)
cpus.tree.1 <- rpart(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data=cpus)
windows(height=8,width=8)
par(mar=c(1,1,1,1), xpd = TRUE)
plot(cpus.tree.1, uniform=F)
text(cpus.tree.1, use.n=T, cex=0.8)

pred.err <- log10(cpus$perf) - predict(cpus.tree.1)
mean(pred.err*pred.err)

subsample <- sample(1:209,replace=T)
cpus.train <- cpus[subsample,]
cpus.test <- cpus[-subsample,]
cpus.tree.1 <- rpart(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data=cpus.train, control = rpart.control(cp=0.02))
pred.err <- log10(cpus.test$perf) - predict(cpus.tree.1,newdata=cpus.test)
mean(pred.err*pred.err)
length((1:209)[-subsample])

# end



