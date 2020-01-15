# election data

library(poLCA)
data(election)
str(election)

set.seed(123)
f.2 <- cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB) ~ 1
lca.2 <- poLCA(f.2,election,nclass=3,nrep=5,na.rm=FALSE) 

subset <- (lca.2$predclass != 3)
election$Z.1 <- lca.2$predclass-1
logistic <- glm(Z.1 ~ AGE + GENDER + EDUC + PARTY, data=election[subset,],family=binomial())
summary(logistic)

# end