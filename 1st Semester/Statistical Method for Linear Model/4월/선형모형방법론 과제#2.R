#part1

A <- matrix( c( 3,-1,1,
                -1,5,-1,
                1,-1,3) ,3,3 , byrow=T)
eigen(A)

#install.packages("matrixcalc") #양정치행렬을 확인하기 위한 패키지
#library(matrixcalc)
isrm <- function(X){ #inverse square root matrix
  
  if (is.positive.definite(X)) {
    if (is.symmetric.matrix(X)) {
    
    ei <- eigen(X)
    
    lambda <- ei$values ; half_inv_lambda <- 1/sqrt(lambda) ; lamb <- diag(half_inv_lambda)
    vec <- ei$vectors
    
    
    half_inv_V <-  vec %*% lamb %*% t(vec)
    
    }
  }
  
  return(half_inv_V)
} 

#check
solve(A) #answer
invinv_v <- isrm(A) ; ((inv_v <- invinv_v %*% invinv_v)) #my answer

#7. 
rm(list=ls())
gc()

biomass<-read.table("C:/Users/82104/Desktop/biomass.txt",header=T)
#str(biomass) ; summary(biomass)

Y <- as.matrix(biomass[,3])
X <- as.matrix(biomass[,4:8])
X0 <- rep(1,length(Y))
X <- cbind(X0,X)
pairs(biomass[,3:8])
points.lines <- function(x, y)
{
  points(x, y)
  lines(loess.smooth(x, y, 0.90))
}
par(pch=18, mkh=.15, cex=0.2, lwd=3)
pairs(biomass[ ,-(1:2)], panel=points.lines)
round(cor(biomass[-(1:2)]),4)


qr_decomp <- qr(X) # QR decomposition
qr_decomp$rank


Q <- qr.Q(qr_decomp,complete=F) #complete : F is default
R <- qr.R(qr_decomp,complete=F)
(( ans <- backsolve(R,t(Q) %*% Y) ))

#https://rfriend.tistory.com/151 참고.
# Specify plotting symbol and size of graph in inches.
# pch=18 requests a filled diamond as a plotting symbol
# mkh=b requests plotting symbols that are b inches high
# cex=c sets the size of the characters used to
# print labels at c times the printer default
# mar mar=c(5,5,4,2) specifies the number of lines
# of text on each side of the plot, starting
# at the bottom and moving clockwise. Here
# write up to 5 lines at the bottom, up to
# 5 lines on the left side, etc

par(pch=18,mkh=.1,cex=1.5,mar=c(5,5,4,2))
b <- solve(t(X)%*%X)%*%t(X)%*%Y
yhat <- X%*%b
e <- Y-yhat
plot(yhat,e,xlab="Predicted Values",ylab="Residuals", main="Residual Plot")

par(mfrow=c(3,2),pty="s",mai=c(0.6,0.2,0.2,0.2)) # pty : each plot has square size 
                                                 # mai : each plot has free space four sides

plot(biomass$salinity,e, xlab="Salinity",ylab="Residuals",
     main="Residual Plot")
lines(loess.smooth(biomass$salinity, e, 0.90))
plot(biomass$pH,e, xlab="pH",ylab="Residuals",
     main="Residual Plot")
lines(loess.smooth(biomass$pH, e, 0.90))
plot(biomass$K,e, xlab="K",ylab="Residuals",
     main="Residual Plot")
lines(loess.smooth(biomass$K, e, 0.90))
plot(biomass$Na,e, xlab="Na",ylab="Residuals",
     main="Residual Plot")
lines(loess.smooth(biomass$Na, e, 0.90))
plot(biomass$Zn,e, xlab="Zn",ylab="Residuals",
     main="Residual Plot")
lines(loess.smooth(biomass$Zn, e, 0.90))


reset_par <- par(no.readonly=T) #store the current par() . If you want to reset par , click the broom on R plot.
par(reset_par) #call the current par()
qqnorm(e, main=" Normal Probability Plot ")
qqline(e)

ll <- lm(biomass$biomass ~ X )
anova(ll)
crossprod(e)/39
product(e,e)


case<-1:45
heading <- c("Case","Salinity", "pH", "K", "Na", "Zn",
             "Biomass", "Predicted", "Residuals")
temp <- cbind(case, X[ ,-1], Y, yhat, e)
dimnames(temp) <- list(case, heading) #dimanames로 매트릭스의 행,열이름을 리스트로 뽑아냄 그걸 케이스와 헤딩으로설정
round(temp,4) #그래도 temp class는 매트릭스로 유지.


s2 <- as.vector(crossprod(e)/39) # 39 is n - r(X)
(( covmat_b<-round(solve(t(X)%*%X)*s2,4) ))
(( sd_b<-round(sqrt(diag(covmat_b)),4) ))



#part2
rm(list=ls())
gc()
W <- read.table("C:\\Users\\82104\\Desktop\\hw2p2.txt",header= T)
Y <- as.matrix(W[ ,2 ])
beta0<-rep(1,length(Y))
X <- as.matrix(cbind(beta0,W[ ,3:5]))

#install.packages("MASS")
library(MASS)
M <- ginv(t(X)%*%X)


A <- t(X) %*% X
list(A =A , M= M) # we know the A & M matrices : A is (X^t)X , M  is moore-penrose inverse
A %*% M %*% A #the answer is A
M %*% A %*% M #the answer is M
i1 <- M %*% A ; i2<- t(M %*% A) ; list(MA = i1,t_MA = i2) #symmetric
j1 <- A %*% M ; j2<- t(A %*% M) ; list(AM = j1,t_AM = j2) #symmetric

(( b <- M %*% t(X) %*% Y )) #gamma hat

temp <- X[ ,4]
groups <- as.factor(W[ ,3]+2*W[ ,4])
lm.out <- lm(Y~groups+temp)
# Plot the observations on the same plot
# with the estimated lines.
# First construct the axes for the plot
# and print titles and labels.
par(mar=c(5,5,4,2),cex=1.5,mkh=1.3)
plot(c(min(temp),max(temp)), c(min(Y),max(Y)),
     xlab='Temperature-100',ylab='Yield',
     type="n", main="Part2- 2 on Assignment 2 ")
# Now insert the observations and fitted lines
points(temp[1:5],Y[1:5],pch=1)
points(temp[6:10],Y[6:10],pch=16)
lines(temp[1:5],lm.out$fitted[1:5],lty=1)
lines(temp[6:10],lm.out$fitted[6:10],lty=6)

#기존에 사용한 회귀직선 추정량들
temp <- X[ ,4]
groups <- as.factor(W[ ,3]+2*W[ ,4])
lm.out <- lm(Y~groups+temp)
summary(lm.out)

plot(c(min(temp),max(temp)), c(min(Y),max(Y)),
     xlab='Temperature-100',ylab='Yield',
     type="n", main="Part2- 2 on Assignment 2 ")
points(temp[1:5],Y[1:5],pch=1)
points(temp[6:10],Y[6:10],pch=16)

b<- as.vector(b)
g1_fitted <- b[1]+b[2]+b[4]*temp[1:5] #group 1 -A
g2_fitted <- b[1]+b[3]+b[4]*temp[6:10] #group 2 -B

lines(temp[1:5],g1_fitted,lty=1)
lines(temp[6:10],g2_fitted,lty=6)






