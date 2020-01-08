rm(list = ls())
library(ISLR)
library(glmnet)

# data
Hitters <- na.omit(Hitters)
x <- model.matrix(Salary~. ,Hitters)[,-1]
y <- Hitters$Salary

p <- ncol(x) - 1
n <- nrow(x)

# least squared estimate
beta.ols <- coef(lm(y ~ x))

# LASSO
grid <- 10^seq(5, -2, length = 100)
lasso.obj <- glmnet(x, y, alpha = 1, lambda = grid)
ridge.obj <- glmnet(x, y, alpha = 0, lambda = grid)
elast.obj <- glmnet(x, y, alpha = 0.5, lambda = grid)

cv.lasso <- cv.glmnet(x, y, alpha = 1, lambda = grid)
cv.ridge <- cv.glmnet(x, y, alpha = 0, lambda = grid)
cv.elast <- cv.glmnet(x, y, alpha = 0.5, lambda = grid)

plot(cv.lasso)
plot(cv.ridge)
plot(cv.elast)

