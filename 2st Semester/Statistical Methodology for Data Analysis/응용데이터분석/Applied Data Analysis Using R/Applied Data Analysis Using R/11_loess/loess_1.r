# Chapter 11. loess of EEE

EEE <- read.table("EEE.txt",header=T)
attach(EEE)
EEE.1 <- loess(NOx ~ E)
order <- order(E)
plot(NOx ~ E, ylim=c(0,6), xlim=c(0.6,1.3), main ="Span = 0.75")
par(new=T)
plot(EEE.1$fit[order] ~ E[order], type="l", ylab="", xlab="", ylim=c(0,6), xlim=c(0.6,1.3))

EEE.2 <- loess(NOx ~ E, span=1)
plot(NOx ~ E, ylim=c(0,6), xlim=c(0.6,1.3), main ="Span = 1.00")
par(new=T)
plot(EEE.2$fit[order] ~ E[order], type="l", ylab="", xlab="", ylim=c(0,6), xlim=c(0.6,1.3))

EEE.3 <- loess(NOx ~ E, span=0.75, degree=1)
plot(NOx ~ E, ylim=c(0,6), xlim=c(0.6,1.3), main ="Linear, Span=0.75")
par(new=T)
plot(EEE.3$fit[order] ~ E[order], type="l", ylab="", xlab="", ylim=c(0,6), xlim=c(0.6,1.3))

EEE.3a <- loess(NOx ~ E, span=0.75, degree=2)
plot(NOx ~ E, ylim=c(0,6), xlim=c(0.6,1.3), main ="Quadratic, Span=0.75")
par(new=T)
plot(EEE.3a$fit[order] ~ E[order], type="l", ylab="", xlab="", ylim=c(0,6), xlim=c(0.6,1.3))

EEE.r <- loess(EEE.1$residuals ~ E, span=1)
plot(EEE.1$residuals ~ E, ylim=c(-1,1), xlim=c(0.6,1.3), main ="Residuals")
par(new=T)
plot(EEE.r$fit[order] ~ E[order], type="l", ylab="", xlab="", ylim=c(-1,1), xlim=c(0.6,1.3))

EEE.1

# end