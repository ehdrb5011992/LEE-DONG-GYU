rm(list=ls())
x <- matrix(c(302,80,179,372),byrow=T,2)
dimnames(x) <- list(c('p','n'),c('o','x'))
x

sen <- x[1,1]/sum(x[,1])
sen

spe <- x[2,2]/sum(x[,2])
spe

prob <- 0.1 #유병률
ppv <- sen*prob / { (sen*prob) + (1-spe) * (1-prob) } #양성예측도
npv <- spe*(1-prob) / { (spe*(1-prob)) + (1-sen) * prob } #음성예측도

ppv ; npv ;
