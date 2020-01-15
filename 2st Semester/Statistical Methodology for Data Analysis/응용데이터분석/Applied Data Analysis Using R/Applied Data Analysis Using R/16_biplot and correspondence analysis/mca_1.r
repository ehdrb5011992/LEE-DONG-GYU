## muliple correspondence analysis of tea data

library(FactoMineR)
data(tea); str(tea)
tea.1 <- tea[,c("Tea","How","sugar","how","where","always")]
str(tea.1)

library(MASS)
mca.tea.1 <- mca(tea.1, nf=2, abbrev=T)
mca.tea.1
windows(heigh=8,width=7)
plot(mca.tea.1,cex=0.8,xlim=c(-0.05,0.05),ylim=c(-0.05,0.05),col=c("gray","red"),main="Tea Data")

windows(heigh=8,width=7)
plot(mca.tea.1$rs,type="n",xlab="dim.1",ylab="dim.2",xlim=c(-0.02,0.02),ylim=c(-0.02,0.02),main="Respondents")
text(mca.tea.1$rs+matrix(rnorm(600,0,0.0005),300,2),label=1:300,cex=0.7,col="gray")

windows(heigh=8,width=7)
plot(mca.tea.1$cs,pch=20,xlab="dim.1",ylab="dim.2",xlim=c(-0.05,0.05),ylim=c(-0.05,0.05),main="Answer Categories")
text(mca.tea.1$cs+0.0025,rownames(mca.tea.1$cs),cex=0.8,col="red")

# end