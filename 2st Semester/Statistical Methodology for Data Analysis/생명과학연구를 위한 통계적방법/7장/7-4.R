rm(list=ls())
gc()
library(survival) #log - rank test :: survdiff()

dat <- read.csv('C:\\Users\\82104\\Desktop\\data\\7_4.csv',header=T)

log_rank <- survdiff(Surv(time,censor)~group,data=dat)
logrank_chisq <- log_rank$chisq ; p_val <- 1 - pchisq(logrank_chisq, length(log_rank$n) - 1)
su <- round(rbind(chisq=logrank_chisq, pvalue=p_val),4) ; colnames(su) <- "log_rank"
su


KM_log_rank <- survfit(Surv(time,censor)~group,data=dat)
plot(KM_log_rank,xlab='Month',ylab='Survival',lty=c(1,2),mark.time=T)
legend(0.2,0.2,c('Group A','Group B'),lty=c(1,2))
