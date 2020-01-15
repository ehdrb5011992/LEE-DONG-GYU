# Propensity Score Matching, subclass matching

library(MatchIt)
data(lalonde)
m.out <- matchit(treat ~ re74 + re75 + educ + black + hispan + age, data = lalonde, method = "subclass")
m.lalonde <- match.data(m.out)
y1 <- m.lalonde$re78[m.lalonde$treat==1]
y0 <- m.lalonde$re78[m.lalonde$treat==0]
w1 <- m.lalonde$weights[m.lalonde$treat==1]
w0 <- m.lalonde$weights[m.lalonde$treat==0]
weighted.mean(y1, w1); weighted.mean(y0, w0)

library(weights)
par(mfrow=c(2,1))
wtd.hist(m.lalonde$distance[m.lalonde$treat==1],nclass=20,main="propensity scores [treat]",xlim=c(0,1))
wtd.hist(m.lalonde$distance[m.lalonde$treat==0],nclass=20,main="propensity scores [control]",xlim=c(0,1))

x11(); par(mfrow=c(2,1))
wtd.hist(m.lalonde$distance[m.lalonde$treat==1],weight=w1,nclass=20,main="propensity scores [treat]",xlim=c(0,1))
wtd.hist(m.lalonde$distance[m.lalonde$treat==0],weight=w0,nclass=20,main="propensity scores [control]",xlim=c(0,1))

# end