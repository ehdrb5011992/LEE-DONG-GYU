rm(list=ls())
gc()
mu <- c(-1,0,-3)
sig <- matrix(c(0.75, 0, 0.25,
                  0 , 1,  0,
                0.25, 0, 0.75),3,3,byrow=T)

A <- matrix(c(1.5, 0,-0.5,
               0,  1,  0,
             -0.5, 0, 1.5),3,3,byrow=T)

#E[(y^t)A(Y)] = (mu^t)A(mu) + tr[(A)sig]
E <- as.vector(t(mu) %*% A %*% mu + sum(diag(A %*% sig)))
#V[(y^t)A(Y)] = 4(mu^t)(A)sig(A)(mu) + 2tr[{(A)sig}^2]
V <- as.vector(4*t(mu) %*% A %*% sig %*% A %*% mu + 2*sum(diag((A %*% sig) %*% (A %*% sig) )))

library(matrixcalc)
is.positive.definite(sig)
A %*% A

C<-matrix(c(0,1,0,-1,
            0,1,-2,1),2,4,byrow=T)
X<-matrix(c(0,0,0,0,
            0,1/2,0,0,
            0,0,1/4,0,
            0,0,0,1/2),4,4,byrow=T)
rm(list=ls())
rep(1:3,2)
X<-matrix(c(rep(1,10),rep(1,5),rep(0,5),rep(0,5),rep(1,5),rep(seq(-10,10,5),2))
          ,ncol=4)

rm(list=ls())
dat <- read.table("C:\\Users\\82104\\Desktop\\hw2p2.txt",header=T)
X <- as.matrix(dat[,3:5])
X <- cbind(rep(1,10),X)
y <- c(dat[,2])

library(MASS)
options(digits=3)
XX_<-ginv(t(X)%*%X)

# numerator
P_x <- X %*% XX_ %*% t(X)
a <- 1/7 * t(y) %*% (diag(10)-P_x) %*% y

#denominator
C <- c(0,0,0,1)
beta_hat <- XX_ %*% t(X) %*% y
b <- t( C %*% beta_hat ) %*% solve(C %*% XX_ %*% C) %*% ( C %*% beta_hat )

F <- b/a


# 5-c


rm(list=ls())
dat <- read.table("C:\\Users\\82104\\Desktop\\hw2p2.txt",header=T)
y<- as.matrix(dat[,2])
intercept<-dat[,3]+dat[,4]
catal <- dat[,3]-dat[,4]
temp <- dat[,5]
W <- cbind(intercept,catal,temp)
W <- data.frame(W)
head(W)

ols<-lm(y~intercept+catal+temp -1 , data=W)
summary(ols)

options(digits=8)
(( anova_ols<-anova(ols) ))
#or
# W <- as.matrix(W)
# t(y) %*% (diag(10) - W %*%solve(t(W) %*% W) %*% t(W)) %*% y


W1<-as.matrix(W[,1])
PW1 <- W1 %*% solve(t(W1) %*% W1) %*% t(W1)
R1 <- t(y) %*% PW1 %*% y
R1

W2<-as.matrix(W[,1:2])
PW2 <- W2 %*% solve(t(W2) %*% W2) %*% t(W2)
R2_1 <-  t(y) %*% (PW2 - PW1) %*% y
R2_1
 
W3<-as.matrix(W[,1:3]) 
PW3 <- W3 %*% solve(t(W3) %*% W3) %*% t(W3)
R3_21 <-  t(y) %*% (PW3 - PW2) %*% y
R3_21

C<-c(1,1,20)  
b<-c(30.6,-2.2,0.85)
MSE<-0.6785
  
W<-as.matrix(W)
l<-t(C) %*% b -2.364 * sqrt(MSE*(1+t(C) %*% solve (t(W) %*% W) %*% C ))
r<-t(C) %*% b +2.364 * sqrt(MSE*(1+t(C) %*% solve (t(W) %*% W) %*% C ))

#5-(h)
V <- (diag(10)-PW3) * MSE #PW3 is PW


l<- r <-rep(0,10)
b <- ols$residuals
z <- 1.96

for (i in 1:10) {
  room<-rep(0,10)
  room[i]<-1
  l[i] <- t(room) %*% b - z * sqrt(t(room) %*% V %*% room)
  r[i] <- t(room) %*% b + z * sqrt(t(room) %*% V %*% room)

  
}

cbind(b,l,r) #b= residual , l = left , r= right


dat2 <- dat[3:5]
y
lm(y~. ,data=dat2)
ols

z1 <- c(rep(1,10)) ; z2 <- c(rep(1,5),rep(0,5))
z3 <- c(rep(0,5),rep(1,5)); z4<-c(seq(-10,10,5),rep(0,5))
z5<- c(rep(0,5),seq(-10,10,5))
Z <- matrix(c(z1,z2,z3,z4,z5),ncol=5)



n <- 1:15
d2 <- 5*n/4
power <- 1 - pf(qf(.95,2,7*n),1,7*n,d2)

par(cex=1.2,mex=1.5,lwd=4,
    mar=c(5.1,4.1,4.1,2.1))

plot(c(0,15), c(0,1), type="n",
     xlab="Sample Size",ylab="Power",
     main="Power versus Sample Size\n
        F-test for equal means")

lines(n, power, type="l",lty=1)
abline(h=0.9, col="red")
abline(v=7,col="red")
abline(v=8,col="red")







