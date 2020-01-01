
# This file is stored as   cement.r

# The data file is stored under the name 
# cement.dat.  It has variable names on the 
# first line.  We will enter the data into
# a data frame.

cement <- read.table("c:/stAT504/cement.txt",header=T)
cement

#  Compute correlations and round the results
#  to four significant digits

round(cor(cement[-1]),4)

#  Create a scatterplot matrix with smooth
#  curves.  Unix users  should first use motif( ) 
#  to open a graphics wundow

points.lines <- function(x, y)
  {
	 points(x, y)
	 lines(loess.smooth(x, y, 0.90))
  }

 par(pch=18, mkh=.15, cex=1.2, lwd=3) 
 pairs(cement[ ,-1], panel=points.lines)



#  Fit a  linear regression model (Venables and
#  Ripley, Chapter 6)

cement.out <- lm(Y~X1+X2+X3+X4, cement)

summary(cement.out)
anova(cement.out)


#  Create a function to evaluate an orthogonal 
#  projection matrix.  Then create a function
#  to compute type II sums of squares.
#  This uses the ginv( )  function in the MASS
#  library, so you must attach the MASS library


library(MASS)


#=======================================
# project( )
#--------------
# calculate orthogonal projection matrix
#=======================================
project <- function(X)  
    { X%*%ginv(crossprod(X))%*%t(X)  }
#=======================================



#========================================
# typeII.SS( )
#------------------
# calculate Type II sum of squares
#
# input  lmout = object made by the lm( ) function
#                y = dependent variable     
# 
#========================================

typeII.SS <- function(lmout,y)
{  
	                      #  generate the model matrix
   model <- model.matrix(lmout)        

                            #  create list of parameter names
   par.name <- dimnames(model)[[2]] 

                            # compute number of parameters
   n.par <- dim(model)[2]             
 
                            # Compute residual mean square
  	SS.res <- deviance(lmout)       
	df2    <- lmout$df.resid             
	MS.res <- SS.res/df2  
	             
	result <- NULL            # store results
	
	                                # Compute Type II SS
	for (i in 1:n.par) {
	    A <- project(model)-project(model[,-i])
	    SS.II  <- t(y) %*% A %*% y
	    df1    <- qr(project(model))$rank - 
	                         qr(project(model[ ,-i]))$rank
	    MS.II  <- SS.II/df1
	    F.stat <- MS.II/MS.res
	    p.val  <- 1-pf(F.stat,df1,df2)
	    temp   <- cbind(df1,SS.II,MS.II,F.stat,p.val)
	    result <- rbind(result,temp)
	}
	
 result <- rbind(result,c(df2,SS.res,MS.res,NA,NA))
 dimnames(result) <- list(c(par.name,"Residual"),
  c("Df","Sum of Sq","Mean Sq","F Value","Pr(F)"))
 cat("Analysis of Variance (TypeII Sum of Squares)\n")
 round(result,6)
} 

#==========================================



typeII.SS(cement.out, cement$Y)




# Venables and Ripley have supplied functions 
# studres( ) and stdres( ) to compute studentized 
# and standardized residuals, respectively.
# You must attach the MASS library before
# using these functions.

cement.res <- cbind(cement$Y,cement.out$fitted,
                              cement.out$resid,
                              studres(cement.out),
                              stdres(cement.out))
dimnames(cement.res) <- list(cement$run,
	       	c("Response","Predicted","Residual",
	                   "Stud. Res.","Std. Res."))
round(cement.res,4)	

		
# Produce plots for model diagnostics

par(mfrow=c(2,2))
plot(cement.out)


































