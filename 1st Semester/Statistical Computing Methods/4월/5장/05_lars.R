rm(list=ls())
gc()


library(lars)

data(diabetes)
x <- diabetes$x
y <- diabetes$y


dim(x)
par(mfrow=c(1,1))



# my code
n <- nrow(x)
p <- ncol(x)
y <- y - mean(y)

x <- t(t(x) - apply(x, 2, mean))
normx <- apply(x^2, 2, sum)
x <- t(t(x)/normx)
apply(x, 2, sum)
apply(x^2, 2, sum) #all elements sum of square

# initial value
obj <- matrix(0, p+1, p)
lambda <- NULL
S <- NULL
mu <- rep(0, n)
beta <- rep(0, p)
obj[1,] <- beta

# current correlation
c <- t(x) %*% (y - mu)
C <- max(abs(c))
lambda[1] <- C
S <- which.max(abs(c))

for (j in 1:p)
{
  s <- sign(c[S])

  tilde.x <- t(t(x[,S, drop = F]) * s) #drop option maintains the matrix
                                       #transpose needs to be tilde.x : small s may be vector
  
  # gram matrix
  G <- t(tilde.x) %*% tilde.x
  
  # unit vector
  one <- rep(1, length(S))
  
  # normalizaing constant
  A <- c(1/sqrt(t(one) %*% solve(G) %*% one)) # c = as.vector
  
  # direction for beta
  omega <-  as.numeric(A) * (solve(G) %*% one)
  
  # equiangular vector
  u <- tilde.x %*% omega
  a <- t(x) %*% u
  
  # gamma
  notS <- setdiff(1:p, S) # not S (S's complements)
  if (length(notS) > 0) # if not empty
  {
    gamma.ps <- (C - c[notS]) / (A - a[notS])
    gamma.mn <- (C + c[notS]) / (A + a[notS])
    
    gamma.ps[gamma.ps < 1.0e-8] <- Inf
    gamma.mn[gamma.mn < 1.0e-8] <- Inf
    
    sel.ps <- which.min(gamma.ps)
    sel.mn <- which.min(gamma.mn)
    
    if (gamma.ps[sel.ps] < gamma.mn[sel.mn]) {
      id <- sel.ps
      hat.gamma <- gamma.ps[id]
    }  else {
      id <- sel.mn
      hat.gamma <- gamma.mn[id]
    }
  } else { # if empty for the final step : gamma.ps[sel.ps] == gamma.mn[sel.mn]
    hat.gamma <- C/c(A)
  }
  
  # update model
  mu   <- mu + hat.gamma * u
  
  # update beta
  beta[S] <- beta[S] + hat.gamma * s * omega
  
  obj[j+1,] <- beta
  
  if (j == p) break
  
  # update active set
  sel <- notS[id]
  S <- c(S, sel)
  c <- t(x) %*% (y - mu)
  C <- max(abs(c))
}

obj1 <- obj 
# reference
obj2 <- lars(x,y,type="lar")$beta

cat("difference = ", max(abs(obj1 - obj2)), "\n")
