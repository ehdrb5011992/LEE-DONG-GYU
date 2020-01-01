
# This file is stored as   crystals.r

# The data file is stored under the name 
# crystal.txt.  It has variable names on the 
# first line.  We will enter the data into
# a data frame.

crystals <- read.table("crystals.txt",header=T)
crystals

#  Create a scatterplot matrix and add
#  an estimated regression line later

par(pch=18, mkh=.15, cex=1.2, lwd=3,
    mar=c(5,5,4,2) ) 

plot(crystals$time, crystals$length, type="p", 
     xlab="Time(seconds)",  
     ylab="Axial length (micrometers)", 
     main= " Axial Lengths of Ice Crystals") 


#  Use the lm( ) function to 
#  fit a linear regression model

linear.out <- lm(length~time, crystals)
summary(linear.out)
anova(linear.out)
lines(crystals$time, linear.out$fitted, lty=1,lwd=3)

# Produce residual plots 

qqnorm(linear.out$resid,
         ylab="Residuals", 
         xlab="Standard Normal Quantiles", 
         main="Normal Probability Plot")
qqline(linear.out$resid)

plot(crystals$time, linear.out$resid, type="p", 
     xlab="Time(seconds)",  
     ylab="Residuals", 
     main= "Residual Plot") 





#  Fit a quadratic curve

quad.out <- lm(length~time+I(time^2), crystals)
summary(quad.out)
anova(quad.out)

plot(crystals$time, crystals$length, type="p", 
     xlab="Time(seconds)",  
     ylab="Axial length (micrometers)", 
     main= " Axial Lengths of Ice Crystals") 

lines(crystals$time, fitted(quad.out), lty=3,lwd=3)


# Produce residual plots 

qqnorm(quad.out$resid,
         ylab="Residuals", 
         xlab="Standard Normal Quantiles", 
         main="Normal Probability Plot")
qqline(quad.out$resid)

plot(crystals$time, quad.out$resid, type="p", 
     xlab="Time(seconds)",  
     ylab="Residuals", 
     main= "Residual Plot") 




#  Goodness of fit test for the quadratic model

group<-as.factor(crystals$time)
crystals <-data.frame(crystals,group)
quadfit.out <- aov(length~time+I(time^2)+group, crystals)
anova(quadfit.out)



  































