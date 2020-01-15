# example 1. exponential growth
growth <- read.table("growth.txt",header=T)
str(growth)
attach(growth)
rss <- function(beta){
    beta.1 <- beta[1]; beta.2 <- beta[2]
    fit <- beta.1*(1-exp(-beta.2*x))
    err <- y - fit
    return(sum(err*err))
}
optim(fn=rss, par=c(1,0.5))
optim(fn=rss, par=c(1,0.5), method="L-BFGS-B", lower=c(0,0))

nlrm <- optim(fn=rss, par=c(1,0))
beta <- nlrm$par
beta.1 <- beta[1]; beta.2 <- beta[2]
fit <- beta.1*(1-exp(-beta.2*x))
plot(x,y,pch=21,xlim=c(0,250),ylim=c(0,1),main="exponential growth",ylab="y")
par(new=T); plot(x,fit,pch=19,cex=0.6,col="red",xlim=c(0,250),ylim=c(0,1),ylab="")

# example 2. constant elasticity of substitution
ces <- read.table("ces.txt",header=T)
str(ces)
attach(ces)
rss <- function(beta){
    beta.0 <- beta[1]; beta.1 <- beta[2]; beta.2 <- beta[3]; beta.3 <- beta[4]
    fit <- beta.0 + beta.1*log(beta.2*x1^beta.3+(1-beta.2)*x2^beta.3)
    err <- y - fit
    return(sum(err*err))
}
optim(fn=rss, par=c(1,-1,0.5,-1), method="L-BFGS-B", lower=c(0,-10,0,-10), upper=c(10,0,1,0))

# end