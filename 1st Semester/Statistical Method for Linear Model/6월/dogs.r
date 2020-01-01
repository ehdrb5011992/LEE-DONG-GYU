
# This file is posted as  dogs.r

dogs <- read.table("dogs.dat",
        col.names=c("Drug","Disease","Y"))
dogs$Drug    <- as.factor(dogs$Drug)
dogs$Disease <- as.factor(dogs$Disease)


# Compute sample means 
# and make a profile plot.

means <- tapply(dogs$Y,
    list(dogs$Drug,dogs$Disease),mean)
means


# Set up the axes and title of the 
# profile plot.  

par(cex=1.2,lwd=3,mex=1.5)
x.axis <- unique(dogs$Drug)
matplot(c(1,4), c(-10,50), type="n", 
     xlab="Drug",  ylab="Mean Response", 
     main= "Change in Systolic Blood Pressure") 

#  Add a profile for each soil type

matlines(x.axis,means,type='l',lty=c(1,2,3),lwd=2)     

# Plot points for the individual observations

matpoints(x.axis,means, pch=c(1,16,18))    

# Add a legend to the plot

legend(2.1,49.6,
   legend=c('Disease 1','Disease 2','Disease 3'),
               lty=c(1,2,3),bty='n')


# The default contrast matrices can be changed by
# resetting the contrasts options.  The contr.treatment
# restricts some parameters to be zero.

options(contrasts=c('contr.treatment','contr.ploy'))


# Fit a model with main effects and interaction
# Compute both sets of Type I sums of squares

lm.out1 <- lm(Y~Drug*Disease,data=dogs)
summary.aov(lm.out1, ssType=1)
lm.out1$coef

lm.out2 <- lm(Y~Disease*Drug,data=dogs)
summary.aov(lm.out2, ssType=1)
lm.out2$coef


# Compute Type III sums of squares and F-tests.
# Use the library "car" and "MASS"

library(MASS)
library(car)
options(contrasts=c("contr.helmert","contr.poly"))
lm.out3 <- aov(Y~Drug*Disease,data=dogs)
Anova(lm.out3,type="III") 


# Create a data frame containing the original 
# data and the residuals and estimated means

data.frame(dogs$Disease,dogs$Drug,dogs$Y,
              Pred=lm.out1$fitted,
             Resid=round(lm.out1$resid,3))


# Create residual plots

frame( )
par(cex=1.4,mex=1.0,lwd=3,pch=2,mkh=0.1)
  plot(lm.out1$fitted, lm.out1$resid,
       xlab="Estimated Means",
       ylab="Residuals",
        main="Residual Plot")
  abline(h=0, lty=2, lwd=3)

    qqnorm(lm.out1$resid)
  qqline(lm.out1$resid)


# Create plots for studentized residulas
# You must attach the MASS library
# to have access to the studres( )
# function that computes studentized
# residuals in the following code

library(MASS)

frame( )
par(cex=1.0,mex=1.0,lwd=3,pch=2,
     mkh=0.1,fin=c(6.5,6.5))
  plot(lm.out1$fitted, studres(lm.out1),
       xlab="Estimated Means",
       ylab="Studentized Residuals",
       main="Studentized Residual Plot")
  abline(h=0, lty=2, lwd=3)

  qqnorm(studres(lm.out1),
       main="Studentized Residuals")
  qqline(studres(lm.out1))


