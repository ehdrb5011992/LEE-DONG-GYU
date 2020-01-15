# values data

library(poLCA)
data(values)
f <- cbind(A,B,C,D)~1

set.seed(123)
M2 <- poLCA(f,values,nclass=2)
hist(M2$posterior[,1],nclass=20,main="Posterior",xlab="class 1")

set.seed(123)
M3 <- poLCA(f,values,nclass=3, maxiter=10000)

# end