
# Analyze the penicillin data from Box,
# Hunter, and Hunter.  This code is 
# posted as  penclln.r

# Enter the data into a data frame and 
# change the Batch and Process variables
# into factors 

penclln <- read.table("penclln.dat", 
     col.names=c("Batch","Process","Yield"))
penclln$Batch <- as.factor(penclln$Batch)
penclln$Process <- as.factor(penclln$Process)
penclln


# Construct a profile plot.  
 
 means <- tapply(penclln$Yield,list(penclln$Batch,penclln$Process),mean)
 
 par(cex=1.2,lwd=3,mex=1.5)
 x.axis <- unique(penclln$Batch)
 matplot(c(1,5), c(75,100), type="n", 
      xlab="Batch",  ylab="Yield", 
      main= "Penicillin Production Results") 
 matlines(x.axis,means,type='l',lty=c(1,2,3,4),lwd=3)         
 
 legend(3.5,101, legend=c('Process A','Process B',
     'Process C','Process D'), lty=c(1,2,3,4),bty='n') 


#  Use the  lme( )  function to fit a model
#  with additive batch (random) and process 
#  (fixed) effects and create diagnostic plots.  

library(nlme)

options(contrasts=c("contr.treatment",
    "contr.poly"))

penclln.lme <- lme(Yield ~ Process, 
    random= ~ 1|Batch, data=penclln,
    method=c("REML"))

summary(penclln.lme)
names(penclln.lme)

#  Contruct ANOVA table for fixed effects
anova(penclln.lme)

#  Estimated parameters for fixed effects
coef(penclln.lme)

# BLUP's for random effects
ranef(penclln.lme)

# Confidence intervals for fixed effects
# and estimated standard deviations
intervals(penclln.lme)

# Create a listing of the original data 
# residuals and predicted values

data.frame(penclln$Process,penclln$Batch,
             penclln$Yield,
             Pred=penclln.lme$fitted,
             Resid=round(penclln.lme$resid,3))


# Create residual plots
frame( )
par(cex=1.2,lwd=3,mex=1.5)
  plot(penclln.lme$fitted, penclln.lme$resid,
       xlab="Estimated Means",
       ylab="Residuals",
       main="Residual Plot")
  abline(h=0, lty=2, lwd=3)


  qqnorm(penclln.lme$resid)
  qqline(penclln.lme$resid)








