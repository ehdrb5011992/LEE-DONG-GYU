# robust regression

data(stackloss)
str(stackloss)
m.ls <- lm(stack.loss ~ ., data=stackloss); m.ls$coefficients
m.ls$coefficients
summary(m.ls)

library(MASS)
m.m <- rlm(stack.loss ~ ., data=stackloss); m.m$coefficients
m.lms <- lqs(stack.loss ~ ., data=stackloss, method="lms"); m.lms$coefficients
m.lts <- lqs(stack.loss ~ ., data=stackloss, method="lts", quantile=15); m.lts$coefficients

residuals <- data.frame(ls=m.ls$ residuals, m=m.m$residuals, lms=m.lms$residuals, lts=m.lts$residuals)
boxplot(residuals, main="residuals of stack.loss", ylim=c(-12,12))

library(boot)
lts.coeff <- function(data, indices) lqs(stack.loss ~ ., data=data, method="lts", quantile=15)$coefficients
boot.log <- boot(data=stackloss, statistic=lts.coeff, R=1000)
print(boot.log)
boot.ci(boot.log, t0=boot.log$t0[4], t=boot.log$t[,4], type = c("perc", "bca"))

# end