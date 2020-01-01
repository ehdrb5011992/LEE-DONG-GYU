
# This file is stored as  cane.r

# Enter the data.  Note that the first
# line of this file is a line of data, 
# not a line of variable names.

cane <- read.table("cane.dat",
    col.names=c("Variety","Nitrogen",
                "Yield"))

#  Create factors

cane$V <- as.factor(cane$Variety)
cane$N <- as.factor(cane$Nitrogen)


# Print the data frame

cane


# Compute mean yields for all combinations
# of nitrogen levels and varieties and
# Make a profile plot. 

means <- tapply(cane$Yield,
          list(cane$Nitrogen,cane$Variety),
            mean)


means

#  Set up the profile plot

par(cex=1.2,lwd=3,mex=1.5,mkh=.20)
x.axis <- unique (cane$Nitrogen)
matplot(c(130,270), c(50,80), 
     type="n", xlab="Nitrogen(lb/acre)",  
     ylab="Mean Yield", 
     main= "Sugar Cane Yields") 

#  Add a profile for each soil type

matlines(x.axis,means,type='l',
               lty=c(1,3,5),lwd=3)     

# Plot symbols for the sample means

matpoints(x.axis,means,pch=c(15,16,18))    

# Add a legend to the plot

legend(130,60, legend=c('Variety 1',
          'Variety 2','Variety 3'),
          lty=c(1,3,5),bty='n')


# Fit a model with main effects and 
# interaction effects. Compute both 
# sets of Type I sums of squares.

options(contrasts=c('contr.sum','contr.ploy'))

lm.out1 <- lm(Yield~N*V, data=cane)
anova(lm.out1)

lm.out2 <- lm(Yield~V*N, data=cane)
anova(lm.out2)
summary(lm.out2, correlation=F)
model.matrix(lm.out2)

# Create diagnostic plots
par(mfrow=c(2,2))
plot(lm.out1)


# Create a data frame containing the original 
# data and the residuals and estimated means

data.frame(cane$Nitrogen,cane$Variety,
         cane$Yield,Pred=lm.out1$fitted,
         Resid=round(lm.out1$resid,3))

# Compute Type III sums of squares and 
# corresponding F-tests.

# Generate an identity matrix and a 
# vector of ones

Iden <- function(n) diag(rep(1,n))      
one  <- function(n) matrix(rep(1,n),ncol=1) 

# Compute the transpose of the model 
# matrix for the cell means model

s  <- length(unique(cane$Nitrogen))                  
t  <- length(unique(cane$Variety))   
st <- s*t       
r  <- length(cane$Yield)/(st)
D <- t(kronecker(Iden(st), t(one(r))))

# Least squares estimation

y <- matrix(cane$Yield,ncol=1)
b <- solve(crossprod(D)) %*% crossprod(D,y)
yhat <- D %*% b                           
sse  <- crossprod(y-yhat)   
df2  <- nrow(y) - st                      

c1 <- kronecker( cbind(Iden(s-1),-one(s-1)), 
                 t(one(t)) )
q1 <- t(b) %*% t(c1)%*% solve( c1 %*% 
        solve(crossprod(D)) %*% t(c1))%*% 
          c1 %*% b
df1<- s-1
f  <- (q1/df1)/(sse/df2)
p  <- 1-pf(f,df1,df2)
c1
data.frame(SS=q1,df=df1,F.stat=f,p.value=p)

c2 <- kronecker( t(one(s)), 
            cbind(Iden(t-1),-one(t-1)) )
q2 <- t(b) %*% t(c2)%*%solve( c2 %*% 
        solve(crossprod(D)) %*% t(c2))%*% 
           c2 %*% b
df1<- t-1
f  <- (q2/df1)/(sse/df2)
p  <- 1-pf(f,df1,df2)
c2
data.frame(SS=q2,df=df1,F.stat=f,p.value=p)


c3 <- kronecker( cbind(Iden(s-1),-one(s-1)), 
                cbind(Iden(t-1),-one(t-1)) )
q3 <- t(b) %*% t(c3)%*% solve( c3 %*% 
        solve(crossprod(D)) %*% t(c3))%*% 
           c3 %*% b
df1<- (s-1)*(t-1)
f  <- (q3/df1)/(sse/df2)
p  <- 1-pf(f,df1,df2)
c3
data.frame(SS=q3,df=df1,F.stat=f,p.value=p)

