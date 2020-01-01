# This code is stored in the file  fden.r

#================================================#
# f.density.plot()                           		      
# ---------------------------------------------                          
# Input : degrees of freedom; it can be a vector. 
#         (e.g.) f.density.plot(c(1,10,10,50))
#         creates a plot with density curves 	
#         for (1,10) df and (10,50) df        
# Output: density plots of the central F      	    
#         distribution.                     
#
#################################################

f.density.plot <- function(n)
{	
    # draw x,y axis and title
	
    x <- seq(.001,4,,50)
    plot(c(0,4), c(0,0.8), type="n", 
         xlab="x", ylab="f(x;n)",
	   main="Densities for Central F Distributions")

    # the length of df should be even.
    
    legend.txt <- NULL
    d.f <- matrix(n,ncol=2,byrow=T); r <- dim(d.f)[1]
    for(i in 1:r) 
      {
       lty.num <- i                        # specify the line types.
       f.x <- df(x,d.f[i,1],d.f[i,2])      # calculate density.
       lines(x, f.x, type="l",lty=lty.num) # draw a line.	
	 legend.txt <- c(legend.txt,
	 paste("(",d.f[i,1],",",d.f[i,2],")",sep=""))
      }

    # The following lines are for inserting 
    # legends on plots using a motif 
    # graphics window.
	
         legend(3, 0.7, 
                cex=1.0,
                legend = paste(legend.txt,"df"), 
	          lty = seq(1,by=1,length=r),
                bty = "n" )
}

   par(cex=1.0,mex=1.3,lwd=2)

   f.density.plot(c(4,20))



#=============================================#
# noncen.f.density()                          #
# --------------------                        #
# Input : v,w    degrees of freedom           #
#         delta  non-centrality parameter     #
#        (e.g.) noncen.f.density(5,20,1.5)    #
# Output: Two F density curves on one plot    #
#          (central and noncental).           #
# Note  : You must define the dnf( ) function #
#         before applying this function.      #
#                                             #
###############################################

n.f.density.plot <- function(v,w,delta)
{
   x <- seq(.001,5,length=60)
   cf.x <- df(x,v,w)
   nf.x <- sapply(x,dnf,v,w,delta)

 # For the main title;
   main1.txt <- "Central and Noncentral F Densities \n
                 with"
   df.txt <- paste(paste("(",paste(paste(v,",",sep=""),
           w,sep=""),sep=""),")",sep="")
   main2.txt <- paste(df.txt,
                 "df and noncentrality parameter =")
   main2.txt <- paste(main2.txt,delta)
   main.txt <- paste(main1.txt,main2.txt)


 # create the axes, lines, and legends.
   plot(c(0,5), c(0,0.8), type="n",xlab="x",
          ylab="f(x;v,w)")
     mtext(main.txt, side=3,line=2.2)
     lines(x, cf.x, type="l",lty=1)
     lines(x, nf.x, type="l",lty=3)
     legend(x=1.6, y=0.64,legend="Central F ",cex=0.9,
               lty =1, bty = "n" )
     legend(x=1.6, y=0.56,legend="Noncentral F",cex=0.9,
               lty =3, bty = "n" )
}


#  This function is executed with the following
#  commands.

    par(cex=1.2,mex=1.5,lwd=2,mar=c(5,4,5,2))
    n.f.density.plot(5,20,1.5)
    n.f.density.plot(5,20,3.0)
    n.f.density.plot(5,20,10.0)
    n.f.density.plot(5,20,20.0)
