### 2장 ###


library(poLCA)

#data values
data(values)
f <- cbind(A,B,C,D) ~ 1

# nclass = 2
set.seed(123)
M2 <- poLCA(f,values,nclass=2)
# nclass = 3
set.seed(123)
M3 <- poLCA(f,values,nclass=3,maxiter=10000)

#data election
data(election)
set.seed(1234)
f.1 <- cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,
             MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB)~PARTY
lcrm.1 <- poLCA(f.1,election,nclass=3,nrep=5)


#graph

library(vcd)
colnames(lcrm.1$posterior) <- c('Favor Gore','Neutral','Favor Bush')
ternaryplot(lcrm.1$posterior,cex=0.5,main='election study',
            col=c('red','green','blue')[lcrm.1$predclass])




#logistic 

data(election)
f.2 <- cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,
             MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB) ~ 1

set.seed(123)
lca.2 <- poLCA(f.2,election,nclass=3,nrep=5,na.rm=F)

subset <- (lca.2$predclass != 3)
election$Z.1 <- lca.2$predclass -1
logistic <- glm(Z.1~ AGE + GENDER + EDUC + PARTY , data=election[subset,],family=binomial())
summary(logistic)


### 3장 ###
# Propensity Score Matching 
library(MatchIt)
data(lalonde)
str(lalonde)
summary(lalonde$re78[lalonde$treat==1])
summary(lalonde$re78[lalonde$treat==0])
boxplot(sqrt(re78) ~ treat, data=lalonde, horizontal=T, main="re78", ylab="treat", xlab="sqrt scale")

# nearest neighbor matching
matchit(treat ~ re74 + re75 + educ + black + hispan + age, method = "nearest", data = lalonde)
matchit(treat ~ re74 + re75 + educ + black + hispan + age, data = lalonde, method = "nearest", discard="both", ratio=2)
m.out <- matchit(treat ~ re74 + re75 + educ + black + hispan + age, data = lalonde, method = "nearest", discard="both", ratio=2)
par(mfrow=c(2,1))
hist(m.out$distance[lalonde$treat==1], xlim=c(0,1), nclass=20, main="propensity scores [treat]")
hist(m.out$distance[lalonde$treat==0], xlim=c(0,1), nclass=20, main="propensity scores [control]")

m.lalonde <- match.data(m.out)
str(m.lalonde)
y1 <- m.lalonde$re78[m.lalonde$treat==1]
y0 <- m.lalonde$re78[m.lalonde$treat==0]
w1 <- m.lalonde$weights[m.lalonde$treat==1]
w0 <- m.lalonde$weights[m.lalonde$treat==0]
weighted.mean(y1, w1); weighted.mean(y0, w0)

library(quantreg)
rq(y1 ~ 1, weights=w1, tau=c(0.25,0.5,0.75))
rq(y0 ~ 1, weights=w0, tau=c(0.25,0.5,0.75))


# Propensity Score Matching, subclass matching

library(MatchIt)
data(lalonde)
m.out <- matchit(treat ~ re74 + re75 + educ + black + hispan + age, data = lalonde, method = "subclass")
m.lalonde <- match.data(m.out)
y1 <- m.lalonde$re78[m.lalonde$treat==1]
y0 <- m.lalonde$re78[m.lalonde$treat==0]
w1 <- m.lalonde$weights[m.lalonde$treat==1]
w0 <- m.lalonde$weights[m.lalonde$treat==0]
weighted.mean(y1, w1); weighted.mean(y0, w0)

library(weights)
par(mfrow=c(2,1))
wtd.hist(m.lalonde$distance[m.lalonde$treat==1],nclass=20,main="propensity scores [treat]",xlim=c(0,1))
wtd.hist(m.lalonde$distance[m.lalonde$treat==0],nclass=20,main="propensity scores [control]",xlim=c(0,1))

x11(); par(mfrow=c(2,1))
wtd.hist(m.lalonde$distance[m.lalonde$treat==1],weight=w1,nclass=20,main="propensity scores [treat]",xlim=c(0,1))
wtd.hist(m.lalonde$distance[m.lalonde$treat==0],weight=w0,nclass=20,main="propensity scores [control]",xlim=c(0,1))

### 4장 ###
setwd("C:\\Users\\82104\\Desktop\\Applied Data Analysis Using R\\Applied Data Analysis Using R\\04_optimization")
# example 1. Poisson observations
x <- c(3,10,5,4)
n <- length(x)
n.log.lik <- function(mu) n*mu - sum(x)*log(mu)
optim(n.log.lik, method="L-BFGS-B", par =1, lower=0, upper=100) 

# example 2. Weibull observations
x <- c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)
n <- length(x)
n.log.lik <- function(theta) {
  theta.1 <- theta[1] ; theta.2 <- theta[2]
  temp.1 <- -n*log(theta.1/theta.2) - (theta.1-1)*sum(log(x/theta.2))
  temp.2 <- sum((x/theta.2)^theta.1)     
  return( temp.1 + temp.2)
}
optim(fn=n.log.lik, par=c(1,10), method="L-BFGS-B", lower=c(0,0))
optim(fn=n.log.lik, par=c(1,10), method="Nelder-Mead")

# example 1 (continued)
library(stats4)
x <- c(3,10,5,4)
n <- length(x)
n.log.lik <- function(mu) n*mu - sum(x)*log(mu)
fitted <- mle(n.log.lik, start=list(mu=1), nobs=n)
logLik(fitted)
summary(fitted)
confint(fitted)

# example 2 (continued)
n.log.lik <- function(theta.1,theta.2) {
  temp.1 <- -n*log(theta.1/theta.2) - (theta.1-1)*sum(log(x/theta.2))
  temp.2 <- sum((x/theta.2)^theta.1)     
  return( temp.1 + temp.2)
}
fitted <- mle(n.log.lik,start=list(theta.1=1,theta.2=10),nobs=n)
logLik(fitted)
summary(fitted)
confint(fitted)
vcov(fitted)


# example 1. exponential growth
growth <- read.table("growth.txt",header=T)
str(growth)
attach(growth)
rss <- function(beta){
  beta.1 <- beta[1]; beta.2 <- beta[2]
  fit <- beta.1*(1-exp(-beta.2*x))
  err <- y - fit
  return(sum(err*err))
}
optim(fn=rss, par=c(1,0.5))
optim(fn=rss, par=c(1,0.5), method="L-BFGS-B", lower=c(0,0))

nlrm <- optim(fn=rss, par=c(1,0))
beta <- nlrm$par
beta.1 <- beta[1]; beta.2 <- beta[2]
fit <- beta.1*(1-exp(-beta.2*x))
plot(x,y,pch=21,xlim=c(0,250),ylim=c(0,1),main="exponential growth",ylab="y")
par(new=T); plot(x,fit,pch=19,cex=0.6,col="red",xlim=c(0,250),ylim=c(0,1),ylab="")

# example 2. constant elasticity of substitution
ces <- read.table("ces.txt",header=T)
str(ces)
attach(ces)
rss <- function(beta){
  beta.0 <- beta[1]; beta.1 <- beta[2]; beta.2 <- beta[3]; beta.3 <- beta[4]
  fit <- beta.0 + beta.1*log(beta.2*x1^beta.3+(1-beta.2)*x2^beta.3)
  err <- y - fit
  return(sum(err*err))
}
optim(fn=rss, par=c(1,-1,0.5,-1), method="L-BFGS-B", lower=c(0,-10,0,-10), upper=c(10,0,1,0))


# TSP for protein data

library(TSP)
protein <- read.table("protein.txt",header=T)
str(protein)
country <- protein$Country
X <- scale(protein[,2:10])
D <- dist(X)
D.tsp <- TSP(D,labels=country)
solve_TSP(D.tsp,method="repetitive_nn")

solve_TSP(D.tsp,method="2-opt")

tsp.country <- solve_TSP(D.tsp,method="2-opt")
labels(tsp.country)

D.plus <- insert_dummy(D.tsp, label = "cut")
tsp.country.plus <- solve_TSP(D.plus, method="2-opt")
tsp.country.plus
tsp.country.cut <- cut_tour(tsp.country.plus, "cut")
labels(tsp.country.cut)

mds <- cmdscale(D)
plot(mds[,1],mds[,2],type="n", xlim=c(-5,5),ylim=c(-5,5),main="Protein Consumption")
text(mds[,1],mds[,2],abbreviate(country,minlength=3),cex=0.8)
seq.mds <- as.integer(tsp.country.cut)
for (i in 2:length(country)){
  segments(mds[seq.mds[i-1],1],mds[seq.mds[i-1],2],mds[seq.mds[i],1],mds[seq.mds[i],2],col="red")
}


### 5장 ###

library(mice)
data(nhanes)
str(nhanes)

pattern <- md.pattern(nhanes)

colnames(pattern)[ncol(pattern)] <- "# miss"
rownames(pattern)[nrow(pattern)] <- "# miss"
pattern
md.pairs(nhanes)

mice(nhanes, seed=23109, m=5)

# use data nhanes2

data(nhanes2)
str(nhanes2)
mice(nhanes2, seed=23109, m=5)

imp.mice <- mice(nhanes2, seed=23109, m=10)
print(imp.mice)

imp.mice$imp$bmi
imp.mice$imp$hyp
imp.mice$imp$chl

stripplot(imp.mice, pch=21, cex=1.2, col=c("blue","red"))
xyplot(imp.mice, chl~bmi|.imp, pch=21, col=c("blue","red"), cex=1.4)

fit <- with(imp.mice, lm(chl ~ age + bmi))
print(pool(fit))
round(summary(pool(fit)),2)

fit.hyp <- with(imp.mice, glm(hyp ~ bmi + chl, family=binomial()))
print(pool(fit.hyp))
round(summary(pool(fit.hyp)),2)

summary.bmi <- matrix(0,10,3)
rownames(summary.bmi) <- 1:10
colnames(summary.bmi) <- c("25%","50%","75%")
for (i in 1:10) {
  x <- complete(imp.mice,i)$bmi
  summary.bmi[i,] <- summary(x)[c(2,3,5)]
}
summary.bmi
round(apply(summary.bmi,2,mean),1)



