# This code is stored in:    tden.r

#=================================================#
# t.density.plot()                                #
# ------------------------                    #
# Input : degrees of freedom; it can be a vector. #
#           (e.g.) t.density.plot(c(2,5,7,30))    #
#           creates curves for  df = 2,5,7,and 30 #
# Output: density plot of Student t distribution. #
# Note:  Unix users must first use   motif()      #
#           to open a grpahics window before      #
#           using  this function.                 #
#================================================ #


t.density.plot <- function(df)
{
   x <- seq(-4,4,,100)

   # draw x,y axis and title

   plot(c(-4,4), c(0,.4), type="n",
      xlab="x", ylab="f(x;df)",
      main="Central  t  Densities")

   for(i in 1:length(df)) {
      lty.num <- 2*i-1      # specify the line types.
      f.x <- dt(x,df[i])    # calculate density.
      lines(x, f.x, type="l",lty=lty.num)   # draw.
    }


   # The following code creates a legend;

      legend(2, 0.4,
      legend = paste(as.character(df),"df") ,
      lty = seq(1,by=2,length=length(df)) ,
      bty = "n")
}


#  This function can be executed as follows.
#  Windows users should delete the motif( )
#  command.

   par(cex=1.2,mex=1.5,lwd=4)
   t.density.plot(c(2,5,30))

   t.density.plot(5)