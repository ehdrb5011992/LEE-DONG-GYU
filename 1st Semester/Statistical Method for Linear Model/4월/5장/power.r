# This file is stored in power.r

#===========================================

n <- 2:50
d2 <- n/2
power <- 1 - pf(qf(.95,2,3*n-3),2,3*n-3,d2)

par(cex=1.2,mex=1.5,lwd=4,
mar=c(5.1,4.1,4.1,2.1))

plot(c(0,50), c(0,1), type="n",
        xlab="Sample Size",ylab="Power",
        main="Power versus Sample Size\n
        F-test for equal means")

lines(n, power, type="l",lty=1)
abline(h=0.9, col="red")
abline(v=26,col="red")

#============================================
