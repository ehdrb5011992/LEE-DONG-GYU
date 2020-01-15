# Propensity Score Matching 

library(MatchIt)
data(lalonde)
str(lalonde)
summary(lalonde$re78[lalonde$treat==1])
summary(lalonde$re78[lalonde$treat==0])
boxplot(sqrt(re78) ~ treat, data=lalonde, horizontal=T, main="re78", ylab="treat", xlab="sqrt scale")

# nearest neighbor matching
matchit(treat ~ re74 + re75 + educ + black + hispan + age, method = "nearest", data = lalonde)
matchit(treat ~ re74 + re75 + educ + black + hispan + age, data = lalonde, method = "nearest", discard="both", ratio=2)
m.out <- matchit(treat ~ re74 + re75 + educ + black + hispan + age, data = lalonde, method = "nearest", discard="both", ratio=2)
par(mfrow=c(2,1))
hist(m.out$distance[lalonde$treat==1], xlim=c(0,1), nclass=20, main="propensity scores [treat]")
hist(m.out$distance[lalonde$treat==0], xlim=c(0,1), nclass=20, main="propensity scores [control]")

m.lalonde <- match.data(m.out)
str(m.lalonde)
y1 <- m.lalonde$re78[m.lalonde$treat==1]
y0 <- m.lalonde$re78[m.lalonde$treat==0]
w1 <- m.lalonde$weights[m.lalonde$treat==1]
w0 <- m.lalonde$weights[m.lalonde$treat==0]
weighted.mean(y1, w1); weighted.mean(y0, w0)

library(quantreg)
rq(y1 ~ 1, weights=w1, tau=c(0.25,0.5,0.75))
rq(y0 ~ 1, weights=w0, tau=c(0.25,0.5,0.75))

# end