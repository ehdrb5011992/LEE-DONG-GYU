# quantile regression

library(quantreg)
data(stackloss)
rq(stack.loss ~ ., tau=0.5, data=stackloss) # median(L1) regression  
rq(stack.loss ~ ., tau=c(0.25,0.75), data=stackloss)  # the 1st and 3rd quartile regression 

m.50 <- rq(stack.loss ~ ., tau=0.5, data=stackloss)
boxplot(m.50$residuals, ylab="residuals", main="median reg")

# end

