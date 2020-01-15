# election data

library(poLCA)
data(election)
str(election)

set.seed(1234)
f.1 <- cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB)~PARTY
lcrm.1 <- poLCA(f.1,election,nclass=3,nrep=5) 

library(vcd)
colnames(lcrm.1$posterior) <- c("Democrat","Independent","Republican")
ternaryplot(lcrm.1$posterior,col=c("red","green","blue")[lcrm.1$predclass],cex=0.5,main="election study")

# end