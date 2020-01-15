# Chapter 5. Missing value imputation using "mice"

library(mice)
data(nhanes)
str(nhanes)

pattern <- md.pattern(nhanes)
colnames(pattern)[ncol(pattern)] <- "# miss"
rownames(pattern)[nrow(pattern)] <- "# miss"
pattern
md.pairs(nhanes)

mice(nhanes, seed=23109, m=5)

# use data nhanes2

data(nhanes2)
str(nhanes2)
mice(nhanes2, seed=23109, m=5)

imp.mice <- mice(nhanes2, seed=23109, m=10)
print(imp.mice)

imp.mice$imp$bmi
imp.mice$imp$hyp
imp.mice$imp$chl

stripplot(imp.mice, pch=21, cex=1.2, col=c("blue","red"))
xyplot(imp.mice, chl~bmi|.imp, pch=21, col=c("blue","red"), cex=1.4)

fit <- with(imp.mice, lm(chl ~ age + bmi))
print(pool(fit))
round(summary(pool(fit)),2)

fit.hyp <- with(imp.mice, glm(hyp ~ bmi + chl, family=binomial()))
print(pool(fit.hyp))
round(summary(pool(fit.hyp)),2)

summary.bmi <- matrix(0,10,3)
rownames(summary.bmi) <- 1:10
colnames(summary.bmi) <- c("25%","50%","75%")
for (i in 1:10) {
   x <- complete(imp.mice,i)$bmi
   summary.bmi[i,] <- summary(x)[c(2,3,5)]
}
summary.bmi
round(apply(summary.bmi,2,mean),1)

densityplot(imp.mice, ~bmi|.imp)

# end

