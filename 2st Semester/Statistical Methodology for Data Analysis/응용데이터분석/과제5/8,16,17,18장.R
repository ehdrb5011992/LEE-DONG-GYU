### 8장 ###

# bootstrapping a single statistic: bivariate correlation 

library(boot)
x <- c(15,26,10,9,15,20,18,11,8,20,7,9,10,11,11,10,12,17,11,10)
y <- c(95,71,83,91,102,87,93,100,104,94,113,96,83,84,102,100,105,121,86,100)
cor.X <- function(data, indices) cor(data[indices,1],data[indices,2])

boot.log <- boot(data=cbind(x,y), statistic=cor.X, R=1000)
print(boot.log)

hist(boot.log$t, xlim=c(-1,1), nclass=20, main="bootstrap correlations", xlab="corr")
boot.ci(boot.log, type = c("perc", "bca"))

# end

# bootstrapping several statistics: logistic regression coefficients 

library(boot)
data(Affairs, package = "AER")
str(Affairs)
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
# Affairs$ynaffair <- factor(Affairs$ynaffair, levels = c(0,1), labels = c("No", "Yes"))
logistic.model <- glm(ynaffair ~ gender + age + yearsmarried + religiousness + rating, family=binomial(), data = Affairs)
summary(logistic.model)

coefs <- function(data, indices, formula) {
  data.1 <- data[indices, ]
  fit <- glm(formula, data = data.1, family=binomial())
  return(coef(fit))
}
logistic.log <- boot(data = Affairs, statistic = coefs, R = 1000, formula = ynaffair ~ gender + age + yearsmarried + religiousness + rating)
print(logistic.log, index=1:6, digits=2)

boot.ci(logistic.log, type = c("perc", "bca"), conf = 0.9, index=2)
boot.ci(logistic.log, type = c("perc", "bca"), index=3)
boot.ci(logistic.log, type = c("perc", "bca"), index=4)
boot.ci(logistic.log, type = c("perc", "bca"), index=5)
boot.ci(logistic.log, type = c("perc", "bca"), index=6)

# end

# difference of two sample means in the final two series of the gravity data.

# the case of two simulated gamma samples

library(boot)

set.seed(123)
x <- rgamma(100,5,0.8)
y <- rgamma(100,5,1.0)
x.and.y <- data.frame(x, y)
z <- stack(x.and.y)
t.test(x, y, var.equal=T)

diff.means <- function(data, n1, indices) {
  m1 <- mean(data[indices[indices <= n1],1])
  m2 <- mean(data[indices[indices >  n1],1])
  c(m1 - m2)
}

z.boot <- boot(data=z, statistic=diff.means, n1=100, R=1000, strata=z[,2])
boot.ci(z.boot, type = c("perc", "bca"))

# end


### 16장 ###
setwd("C:\\Users\\82104\\Desktop\\대학원 수업\\1학년 2학기\\통계분석방법론\\응용데이터분석\\Applied Data Analysis Using R\\Applied Data Analysis Using R\\16_biplot and correspondence analysis")

# r biplot for protein data

X.1 <- read.table("protein.txt",header=T)
str(X.1)
X <- X.1[,2:10]
n <- nrow(X)

# Using R function princomp()
pca.X <- princomp(X, cor=T)
pca.X$scores <- pca.X$scores*sqrt((n-1)/n)
# round(pca.X$scores,3)
# round(apply(pca.X$scores,2,sd),3)
biplot(pca.X,scale=0,cex=0.8,xlab="First Dimension",ylab="Second Dimension")
X.1$Country
summary(pca.X,loadings=T)

# end

# Correspondence analysis for 2007 Presidential Election Survey data
F0 <- read.table("Election.txt", header=T)
F <- as.matrix(F0[,-1])
rownames(F) <- F0[,1]
addmargins(F)
r.margin <- addmargins(F)[,7]
c.margin <- addmargins(F)[8,]

# MASS corresp() 
library(MASS)
corresp.F <- corresp(F,nf=2)
attach(corresp.F)
ROW <- rscore%*%diag(cor)
COL <- cscore

plot(ROW[,2]~ROW[,1],type="n",xlim=c(-2,2),ylim=c(-2,2),xlab="First Dimension",ylab="Second Dimension",main="Presidential Election 2007")
text(ROW[,1],ROW[,2],labels=rownames(ROW),cex=1,col="blue")
par(new=T)
plot(COL[,2]~COL[,1],type="n",xlim=c(-2,2),ylim=c(-2,2),xlab="",ylab="")
text(COL[,1],COL[,2],labels=rownames(COL),cex=1,col="red")
x11()
plot(ROW[,2]~ROW[,1],type="n",xlim=c(-2,2),ylim=c(-2,2),xlab="First Dimension",ylab="Second Dimension",main="Presidential Election 2007")
text(ROW[,1],ROW[,2],labels=rownames(ROW),cex=r.margin/sum(r.margin)*12,col="blue")
par(new=T)
plot(COL[,2]~COL[,1],type="n",xlim=c(-2,2),ylim=c(-2,2),xlab="",ylab="")
text(COL[,1],COL[,2],labels=rownames(COL),cex=c.margin/sum(c.margin)*12,col="red")

# Symmetric Plot
x11(); par(mfrow=c(1,1))
plot(rscore[,2]~rscore[,1],type="n",xlim=c(-3.5,3.5),ylim=c(-3.5,3.5),xlab="First Dimension",ylab="Second Dimension",main="Presidential Election 2007")
text(rscore[,1],rscore[,2],labels=rownames(rscore),cex=0.8,col="blue")
par(new=T)
plot(cscore[,2]~cscore[,1],type="n",xlim=c(-3.5,3.5),ylim=c(-3.5,3.5),xlab="",ylab="")
text(cscore[,1],cscore[,2],labels=rownames(cscore),cex=0.8,col="red")

# Alternative Biplot
biplot(rscore,cscore,cex=1,xlab="First Dimension",ylab="Second Dimension",main="Presidential Election 2007")
# end

## muliple correspondence analysis of tea data

library(FactoMineR)
data(tea); str(tea)
tea.1 <- tea[,c("Tea","How","sugar","how","where","always")]
str(tea.1)

library(MASS)
mca.tea.1 <- mca(tea.1, nf=2, abbrev=T)
mca.tea.1
windows(heigh=8,width=7)
plot(mca.tea.1,cex=0.8,xlim=c(-0.05,0.05),ylim=c(-0.05,0.05),col=c("gray","red"),main="Tea Data")

windows(heigh=8,width=7)
plot(mca.tea.1$rs,type="n",xlab="dim.1",ylab="dim.2",xlim=c(-0.02,0.02),ylim=c(-0.02,0.02),main="Respondents")
text(mca.tea.1$rs+matrix(rnorm(600,0,0.0005),300,2),label=1:300,cex=0.7,col="gray")

windows(heigh=8,width=7)
plot(mca.tea.1$cs,pch=20,xlab="dim.1",ylab="dim.2",xlim=c(-0.05,0.05),ylim=c(-0.05,0.05),main="Answer Categories")
text(mca.tea.1$cs+0.0025,rownames(mca.tea.1$cs),cex=0.8,col="red")

# end

### 17장 ###
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


# simulated 2-dim linear SVM regression

set.seed(12345)
x <- rnorm(100)
y <- 0.8*x + rnorm(100,0,0.6)  # runif(100,-1,1)
sd(x)
sd(y)
windows(height=7.5,width=7)
plot(y ~ x, main="simulated bivariate data",xlim=c(-3,3),ylim=c(-3,3))
abline(c(0,0.8),col="blue")
abline(c(-1,0.8),col="red",lty="dotted")
abline(c(1,0.8),col="red",lty="dotted")
for (i in 1:100){
  if (y[i]-0.8*x[i] >  1) segments(x[i],y[i],x[i],0.8*x[i]+1)
  if (y[i]-0.8*x[i] < -1) segments(x[i],y[i],x[i],0.8*x[i]-1)
}

library(e1071)
svm.model <- svm(y ~ x, kernel="linear", epsilon=1, scale=F)  # kernel="radial", epsilon=1, gamma=0.5
summary(svm.model)

windows(height=7.5,width=7)
plot(y ~ x, main="simulated bivariate data",xlim=c(-3,3),ylim=c(-3,3))
par(new=T)
plot(svm.model$fitted ~ x, main="",xlim=c(-3,3),ylim=c(-3,3),xlab="",ylab="",col="red", pch=20)
points(x[svm.model$index],y[svm.model$index],pch=20)

# end

# nonlinear SVM regression

set.seed(12345)
x <- rnorm(100)
y.fit <- 0.8*x^2
y <- y.fit + rnorm(100,0,0.6)  # runif(100,-1,1)

windows(height=7.5,width=7)
plot(y ~ x, main="simulated bivariate data 2",xlim=c(-3,3),ylim=c(-2,6))
par(new=T)
plot(y.fit ~ x, main="",xlim=c(-3,3),ylim=c(-2,6),xlab="",ylab="",col="red", pch=20)
index <- (1:100)[abs(y-y.fit)>=1]
points(x[index],y[index],pch=20,col="blue")

library(e1071)
svm.model <- svm(y ~ x, gamma=0.5, epsilon=1, scale=F)  # kernel="radial", epsilon=1, gamma=0.5
summary(svm.model)

windows(height=7.5,width=7)
plot(y ~ x, main="simulated bivariate data 2",xlim=c(-3,3),ylim=c(-2,6))
par(new=T)
plot(svm.model$fitted ~ x, main="",xlim=c(-3,3),ylim=c(-2,6),xlab="",ylab="",col="red", pch=20)
points(x[svm.model$index],y[svm.model$index],pch=20)

# end


# nonlinear classification using SVM

set.seed(123)
x <- matrix(rnorm(200),100,2)
grp <- ifelse(apply(x*x,1,sum) <= 1.16, 1, 2)
y <- as.factor(grp)
table(grp)

svm.model <- svm(y ~ x, kernel="radial", scale=F, gamma=0.5)
summary(svm.model)

windows(height=8,width=7)
plot(x,pch=c(20,21)[grp],col=c("blue","red")[svm.model$fitted],xlim=c(-3,3),ylim=c(-3,3),main="Simulated Bivariate Data 3",xlab="x1",ylab="x2")
theta <- seq(0,1,0.01)*2*pi
r <- sqrt(1.16)
par(new=T); plot(r*cos(theta),r*sin(theta),lty="dotted",type="l",xlim=c(-3,3),ylim=c(-3,3),xlab="",ylab="")

table(svm.model$fitted)

# end

# SVM of the spam data

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


### 18장 ###


# Chapter 18. 
# Tree model via rpart package
# kyophosis data
data(kyphosis)
str(kyphosis)
table(kyphosis$Kyphosis)

tree.1 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
tree.1
par(mar=c(1,1,1,1), xpd = TRUE); plot(tree.1)
text(tree.1, use.n = TRUE)

tree.2 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, parms = list(split = "information"))
tree.2
par(mar=c(1,1,1,1), xpd = TRUE); plot(tree.2)
text(tree.2, use.n = TRUE)

table(kyphosis$Kyphosis, predict(tree.1, type="class"))
table(kyphosis$Kyphosis, predict(tree.2, type="class"))

kyphosis$wts <- ifelse(kyphosis$Kyphosis=="present", 0.79, 0.21)

tree.2a <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, parms = list(split = "information"), weights=wts)
par(mar=c(1,1,1,1), xpd = TRUE); plot(tree.2a)
text(tree.2a, use.n = TRUE)

table(kyphosis$Kyphosis, predict(tree.2a, type="class"))

# end


# Chapter 18. Tree model via rpart package
# bagging using ipred package
# for kyphosis

data(kyphosis)
kyphosis.present <- kyphosis[kyphosis$Kyphosis=="present",]
kyphosis.absent <- kyphosis[kyphosis$Kyphosis=="absent",]
kyphosis.balanced <- rbind(kyphosis.present, kyphosis.present, kyphosis.present, kyphosis.present, kyphosis.absent)


bag.kyphosis <- bagging(Kyphosis ~ Age + Number + Start, data = kyphosis.balanced, coob=T)
bag.kyphosis
addmargins(table(kyphosis.balanced$Kyphosis,predict(bag.kyphosis)))

# for cpus
data(cpus)
cpus.bag <- bagging(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data=cpus, coob=T, nbagg=100)
cpus.bag

# end



# bagging for cpus data
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


# Chapter 18. Tree model
# random forest model


data(kyphosis); str(kyphosis)
kyphosis.present <- kyphosis[kyphosis$Kyphosis=="present",]
kyphosis.absent <- kyphosis[kyphosis$Kyphosis=="absent",]
kyphosis.balanced <- rbind(kyphosis.present, kyphosis.present, kyphosis.present, kyphosis.present, kyphosis.absent)


RF.1 <- randomForest(Kyphosis ~ Age+Number+Start, data=kyphosis.balanced, var.importance=T)
RF.1$importance

# for cpus data

data(cpus, package="MASS"); str(cpus)
RF.2 <- randomForest(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data=cpus,importance=TRUE)
RF.2
varImpPlot(RF.2)

# end








