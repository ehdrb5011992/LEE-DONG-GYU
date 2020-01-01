
# This file is stored as carrots.r

# First enter the data.  The data file does
#  not have variable names on the first line

carrot <- read.table("carrots.dat",
           col.names=c("Soil","Variety","Days"))
carrot$Soil <- as.factor(carrot$Soil)
carrot$Variety <- as.factor(carrot$Variety)
attach(carrot)


# Compute sample means of germination times 
# for all combinations of soil type and varieties 
# of carrot seeds.  Make a profile plot
# At this point UNIX users should open a
# graphics window with the  motif( ) function

means <- tapply(Days,list(Variety,Soil),mean)
means

#  Set up the axes and title of the profile plot

par(cex=1.2,lwd=3,mex=1.2,mkh=0.15)
x.axis <- unique(Variety)
matplot(c(1,3), c(0,40), type="n", 
     xlab="Variety",  ylab="Mean Time", 
     main= "Time to Carrot Seed Germination") 

#  Add a profile for each soil type

matlines(x.axis,means,type='l',lty=c(1,3),lwd=3) 

# Plot points for the individual observations

matpoints(x.axis,means, pch=c(1,16))    

# Add a legend to the plot

legend(2.2,38.6,
   legend=c('Soil Type 1','Soil Type 2'),
      lty=c(1,3),bty='n')


# Fit a model with main effects and interaction
# Compute both sets of Type I sums of squares

lm.out1 <- lm(Days~Soil*Variety)
anova(lm.out1)

lm.out2 <- lm(Days~Variety*Soil)
anova(lm.out2)


# Create a data frame containing the original 
# data and the residuals and estimated means

data.frame(Soil,Variety,Days,Pred=lm.out1$fitted,
                   Resid=round(lm.out1$resid,3))


#  Make diagnostic plots.  You must attach the MASS
#  library to have access to the studres( ) that
#  computes studentized residuals in the following
#  code.  

 library(MASS)

  par(cex=1.0,mex=1.0,lwd=3,pch=2,mkh=.1)
  par(mfrow=c(1,2))
  plot(lm.out1$fitted, lm.out1$resid,
        xlab="Estimated Means", 
        ylab="Residuals", pch=2, mkh=.1)
   abline(h=0, lty=2, lwd=3)
  
   qqnorm(lm.out1$resid)
   qqline(lm.out1$resid)


# By default R uses so-called Helmert contrast 
# matrices for unordered factors and orthogonal 
# polynomial contrast matrices for ordered factors.

options(contrasts=c('contr.helmert','contr.ploy'))
lm.out <- lm(Days~Soil*Variety) 
model.matrix(lm.out)   
summary(lm.out)
anova(lm.out)


# The default contrast matrices can be changed by
# resetting the contrasts options.  The contr.sum
# option restricts parameters to sum to zero
# across the levels of any single factor.

options(contrasts=c('contr.sum','contr.ploy'))

# Now, ``contr.sum'' will be used for unordered factors 
# and orthogonal polynomial contrast matrices will 
# be used for ordered factors.

lm.out <- lm(Days~Soil*Variety)  
model.matrix(lm.out)
summary(lm.out)
anova(lm.out)

# Compute Type III sums of squares and F-tests.
# Enter the transpose of the model matrix for
# the cell means model.


# Model matrix for the cell means model

comb <- as.factor(10*as.numeric(Soil) + as.numeric(Variety))
lm.out <- lm(Days~comb - 1)
D <- model.matrix(lm.out)
D


# Compute the sample means

y <- matrix(Days,ncol=1)

b <- solve(crossprod(D)) %*% crossprod(D,y)
b

# Generate an identity matrix and a vector of ones

Iden <- function(n) diag(rep(1,n))      
one  <- function(n) matrix(rep(1,n),ncol=1) 


# Compute Type III sums of squares and 
# related F-tests

s  <- length(unique(Soil))                  
t  <- length(unique(Variety))          

yhat <- D %*% b                           
sse  <- crossprod(y-yhat)   
df2  <- nrow(y) - s*t                      

# F-test for Type III sum of squares for 
# Soil main effects

c1 <- kronecker( cbind(Iden(s-1),-one(s-1)), t(one(t)) )
q1 <- t(b) %*% t(c1)%*% 
            solve( c1 %*% solve(crossprod(D)) %*% t(c1))%*% 
            c1 %*% b
df1<- s-1
f  <- (q1/df1)/(sse/df2)
p  <- 1-pf(f,df1,df2)
c1
data.frame(SS=q1,df=df1,F.stat=f,p.value=p)


# F-test for Type III sum of squares for 
# Variety main effects

c2 <- kronecker( t(one(s)), cbind(Iden(t-1),-one(t-1)) )
q2 <- t(b) %*% t(c2)%*% 
           solve( c2 %*% solve(crossprod(D)) %*% t(c2))%*% 
              c2 %*% b
df1<- t-1
f  <- (q2/df1)/(sse/df2)
p  <- 1-pf(f,df1,df2)
c2
data.frame(SS=q2,df=df1,F.stat=f,p.value=p)


# F-test for Type III sum of squares for 
# Soil x Variety interaction effects


c3 <- kronecker( cbind(Iden(s-1),-one(s-1)), 
                cbind(Iden(t-1),-one(t-1)) )
q3 <- t(b) %*% t(c3)%*% 
            solve( c3 %*% solve(crossprod(D)) %*% t(c3))%*% 
               c3 %*% b
df1<- (s-1)*(t-1)
f  <- (q3/df1)/(sse/df2)
p  <- 1-pf(f,df1,df2)
c3
data.frame(SS=q3,df=df1,F.stat=f,p.value=p)


# Easy way to get TYPE III test results
# Attach the package "car"

install.packages("car")
library(car)

Anova(lm(Days~Soil*Variety),type="III")


