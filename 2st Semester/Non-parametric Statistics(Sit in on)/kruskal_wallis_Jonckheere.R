#kruskal Wallis

###################

rm(list=ls())
H=0
n <- c(1,2,2)

t=100000
for (i in 1:t) {
  x <- sample(5)  
  r_1<- x[1] ; r_2 <- x[2:3] ; r_3 <- x[4:5]
  r <- c(sum(r_1),sum(r_2),sum(r_3))
  N <- sum(n)
  H[i] <- 12/{N*(N+1)} *sum(r^2/n) - 3*(N+1)
}
H <- round(H,3)
data <- as.factor(H)
prop<-  table(data) / t
1 - cumsum(prop)


#Jonckheere


###################

N <- 7
rank <- rep(1:N)
all <- choose(7, 2) * choose(5, 2)
M <- matrix(0, all, N)

name1 <- rep(1:7)
x1 <- combinations(7, 2, name1)
for (i in 1:21) {
  name2 <- name1[-x1[i, ]]
  x2 <- combinations(5, 2, name2)
  tem <- cbind(matrix(x1[i, ], 10, 2, byrow = TRUE), x2)
  temp <- matrix(0, 10, 3)
  for (j in 1:10) {
    name3 <- name1[-tem[j, ]]
    temp[j,] <- name3
  }
  x3 <- cbind(tem, temp)
  a <- 10*i-9
  b <- 10*i
  M[a:b, ] <- x3
}

J <- rep(0, all)
for(i in 1:all) {
  d <- M[i, ]
  s1 <- d[1:2]; s2 <- d[3:4]; s3 <- d[5:7]
  U12 <- sum(I(s1[1] < s2) + I(s1[2] < s2))
  U13 <- sum(I(s1[1] < s3) + I(s1[2] < s3))
  U23 <- sum(I(s2[1] < s3) + I(s2[2] < s3))
  J[i] <- U12 + U13 + U23
}

data <- as.factor(J)
prop<-  table(data) / all
ptable <- 1 - cumsum(prop); ptable


