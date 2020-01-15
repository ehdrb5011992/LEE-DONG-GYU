# bootstrapping several statistics: logistic regression coefficients 

library(boot)
data(Affairs, package = "AER")
str(Affairs)
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
# Affairs$ynaffair <- factor(Affairs$ynaffair, levels = c(0,1), labels = c("No", "Yes"))
logistic.model <- glm(ynaffair ~ gender + age + yearsmarried + religiousness + rating, family=binomial(), data = Affairs)
summary(logistic.model)

coefs <- function(data, indices, formula) {
    data.1 <- data[indices, ]
    fit <- glm(formula, data = data.1, family=binomial())
    return(coef(fit))
}
logistic.log <- boot(data = Affairs, statistic = coefs, R = 1000, formula = ynaffair ~ gender + age + yearsmarried + religiousness + rating)
print(logistic.log, index=1:6, digits=2)

boot.ci(logistic.log, type = c("perc", "bca"), conf = 0.9, index=2)
boot.ci(logistic.log, type = c("perc", "bca"), index=3)
boot.ci(logistic.log, type = c("perc", "bca"), index=4)
boot.ci(logistic.log, type = c("perc", "bca"), index=5)
boot.ci(logistic.log, type = c("perc", "bca"), index=6)

# end