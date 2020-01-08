####2번문제에 대한 코드
#데이터 생성
set.seed(1)
n<-100;p<-4
x<-matrix(rnorm(n*p,0,1),nc=p,byrow=T)
true.beta<-rep(c(1,-1),2)
y<-sign(x%*%true.beta+rnorm(n,0,0.01))

#####3번
y1<-rexp(50,rate=0.2);y1
y2<-sample(c(1:8),50,replace=T);y2
y<-c(y1,y2)
delta<-c(rep(1,50),rep(0,50))


library("kernlab")
#####4ㅂ

set.seed(1)
x <- matrix(rnorm(n*p), n, p) # generate predictor
x <- cbind(rep(1, n), x)

true.beta <- rep(c(1,-1)/2, (p+1)/2) # true beta
true.eta <- x %*% true.beta   # true eta (linear term)
true.mui<- exp(true.eta)
y <- rpois(n,true.mui)

obj.mle <- glm(y ~ x - 1, family = "poisson")
obj.mle$coefficients


##4

t<-155
n <- 100 # sample size
p <- 5   # predictor dimension
set.seed(t)
x <- matrix(rnorm(n*p), n, p) # generate predictor
x <- cbind(rep(1, n), x)
#true.beta <- c(0,0,0,0,0,0)
true.beta <- rep(c(1,-1)/2, (p+1)/2) # true beta
true.eta <- x %*% true.beta   # true eta (linear term)
true.mui<- exp(true.eta)
y <- rpois(n,true.mui)

set.seed(t)
x <- matrix(rnorm(n*p), n, p) # generate predictor


obj <- mcmc(x,y)
bayes.ci(obj)
boot.ci(x,y)




######################################################### 예상실행###

rm(list=ls())
gc()

source('C:\\Users\\82104\\Desktop\\ST509_Final_2019020314.R')
########################################################
set.seed(1)
x<-rnorm(100,0,1)
set.seed(1)
y<-2*as.numeric((x+rnorm(100,0,0.5))>0)-1

lum(x,y)

#########################################################
library(kernlab)
set.seed(1)
n<-100;p<-4
x<-matrix(rnorm(n*p,0,1),nc=p,byrow=T)
true.beta<-rep(c(1,-1),2)
y<-sign(x%*%true.beta+rnorm(n,0,0.01))

wsvm(x,y)
#########################################################
set.seed(3)
y1<-rexp(50,rate=0.3)
y2<-sample(c(1:8),50,replace=T)
y<-c(y1,y2)
delta<-c(rep(1,50),rep(0,50))

em(y,delta); sum(y)/50

#########################################################
set.seed(3)

n <- 200 # sample size
p <- 5   # predictor dimension

x <- matrix(rnorm(n*p), n, p) # generate predictor
X <- cbind(rep(1, n), x)      # design matrix

beta <- rep(1, p+1) # true beta

eta <- X %*% beta   # true eta (linear term)

lamda <- exp(eta) # pi = mu = E(y|x)
y <- rpois(n, lamda)         # generate reponse
cbind(beta)

mcmc(x,y)
obj <- mcmc(x,y)
bayes.ci(obj)
boot.ci(x,y)
