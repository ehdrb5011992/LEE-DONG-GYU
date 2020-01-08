#2019020314 통계학과 이동규 codes
#1-(d) 

my.posreg <- function( X , y , beta0 = NULL , eps = 1.0e-5, max.iter=100) {
  
  if (is.null(beta0)) beta0 <- rep(0,ncol(X))
  beta <- beta0
  
  if (ncol(X) != length(beta0)) stop("Please check your beta dimension")
  
  for (iter in 1: max.iter) {
    
    #poi dist.
    
    eta <- X %*% beta
    mu <- exp(eta)
    w <- c(mu)
    z <- X %*% beta + (y-mu)/w
    
    
    #solution of normal equation
    tilde.X <- X * sqrt(w)
    tilde.y <- z * sqrt(w)
    qr.obj <- qr(tilde.X)
    Q<-qr.Q(qr.obj) ; R<-qr.R(qr.obj)
    
    new.beta <- backsolve(R,qr.qty(qr.obj,tilde.y))
    # = new.beta <- c(backsolve(R,t(Q) %*% tilde.y))
    # = new.beta <- backsolve(qr.obj$qr, qr.qty(qr.obj, tilde.y)) 
    
    if ( max(abs(new.beta - beta)) < eps  ) break
    beta <- new.beta
    
  }
  
  if (iter == max.iter) warning("Algorithm may not  be converged")
  
  beta.mle <- c(beta)
  return(beta.mle) #mle of beta
}


#########################################################################################


#2-(c) 
my.reg.enet <- function( X , y , gamma0 = NULL , beta0 = NULL , 
                         lambda , alpha = 0.5 , eps = 1.0e-5 , max.iter= 100) {
  
  if ( (alpha > 1) | (alpha < 0)) stop("Please input the alpha : 0<= alpha <= 1")
  if (ncol(X) != length(beta0)) stop("Please check your beta dimension")
  
  # set soft-thresholding function
  Soft <- function(b, lambda) {
    (b - lambda) * (b > lambda) + 
      (b + lambda) * (b < -lambda) + 
      0 * (abs(b) <= lambda)
  }
  
  n <- nrow(X)
  p <- ncol(X) #  == length(beta0) 
  
  if (is.null(beta0)) beta0 <- rep(0,p)
  if (is.null(gamma0)) gamma0 <- c(0)
  
  
  # marginal standardization of x 
  bar.X <- apply(X, 2, mean)
  s <- sqrt(apply(X, 2, var) * (n-1)/n )
  z <- t((t(X) - bar.X)/s) 
  
  # centering of y
  u <- (y - mean(y))
  
  # CD algorithm for Elastic net
  
  for (iter in 1:max.iter) {
    
    #residuals
    beta <- beta0 * s
    r <- u - z %*% beta
    new.beta <- beta                       # beta & new.beta : (t)th, (t+1)th calculated beta in FOR systax each. 
    
    
    for (j in 1:p) {
      
      temp <- beta[j] + c( crossprod(z[,j], r)/n )
      numerator <- Soft(temp,lambda*alpha)
      denominator <- 1 + lambda*(1-alpha)
      
      new.beta[j] <- numerator / denominator
      r <- r - (new.beta[j] - beta[j]) * z[,j]
    }
    
    updated.beta <- new.beta /s 
    delta <- max(abs(updated.beta - beta0)) # beta0 & updated.beta : beta_star's (t)th , (t+1)th each.
    
    if (delta < eps ) break
    
    beta0 <- updated.beta
  }
  
  if (iter == max.iter) warning("Algorithm may not be converged")
  
  beta.enet <- updated.beta
  gamma.enet <- mean(y) - crossprod(beta.enet, bar.X)
  
  
  return(c(gamma.enet , beta.enet)) #enet-penalized solution
  
}


#########################################################################################


#3-(a)
my.posreg.enet <- function ( X , y , beta0 = NULL , lambda ,
                             alpha = 0.5, eps = 1.0e-5 , max.iter=100) {
  
  if ( (alpha > 1) | (alpha < 0)) stop("Please input the alpha : 0<= alpha <= 1")
  if (ncol(X) != length(beta0)) stop("Please check your beta dimension")
  
  # set soft-thresholding function
  Soft <- function(b, lambda) {
    (b - lambda) * (b > lambda) + 
      (b + lambda) * (b < -lambda) + 
      0 * (abs(b) <= lambda)
  }
  
  n <- nrow(X)
  p <- ncol(X) #  == length(beta0) 
  
  if (is.null(beta0)) beta0 <- rep(0,p)
  
  #poi dist.
  for (iter in 1:max.iter) {
    
    eta <- X %*% beta0
    w <- mu <- c( exp(eta) )
    z <- eta + (y-mu)/w
    tilde.X <- sqrt (diag(w)) %*% X # = W^(1/2)X
    tilde.z <- sqrt (diag(w)) %*% z
    
    #stnadardized 
    bar.tilde.X <- apply(tilde.X, 2, mean)
    s.tilde.X <- sqrt(apply(tilde.X, 2, var) * (n-1)/n)
    str.tilde.X <- t((t(X) - bar.tilde.X)/s.tilde.X)
    
    beta.work <- beta0 * s.tilde.X
    r <- tilde.z - str.tilde.X %*% beta.work
    
    #CD
    updated.beta.work <- beta.work
    
    for (j in 1:p) {
      temp <-  beta.work[j] + c( crossprod( str.tilde.X[,j] , r ) /n )
      numerator <- Soft(temp, lambda*alpha )
      denominator <- 1 + lambda*(1-alpha) 
      
      updated.beta.work[j] <- numerator / denominator
      r <- r-(updated.beta.work[j] - beta.work[j]) * str.tilde.X[,j]
      
    }
    new.beta <- updated.beta.work / s.tilde.X
    delta <- max(abs(new.beta - beta0))
    
    if (delta < eps ) break
    
    beta0 <- new.beta
  }
  
  if (iter == max.iter) warning("Algorithm may not be converged")
  
  beta.enet <- new.beta
  return(beta.enet) #enet-penalized solution
  
}




#########################################################################################


#3-(b)
my.posreg.path.enet <- function (X , y , beta0 = NULL , lambda = 2^(-10:10) ,
                                 alpha = 0.5, eps = 1.0e-5 , max.iter=100) {
  
  if ( (alpha > 1) | (alpha < 0)) stop("Please input the alpha : 0<= alpha <= 1")
  if (is.vector(lambda)==F) stop("Please input the lambda with type of vector") 
  if (ncol(X) != length(beta0)) stop("Please check your beta dimension")
  
  # set soft-thresholding function
  Soft <- function(b, lambda) {
    (b - lambda) * (b > lambda) + 
      (b + lambda) * (b < -lambda) + 
      0 * (abs(b) <= lambda)
  }
  
  n <- nrow(X)
  p <- ncol(X) 
  k<-length(lambda) 
  beta.enet.matrix <- matrix(0,k,p)
  
  if (is.null(beta0)) beta0 <- rep(0,p)
  
  
  for (la in 1:k) {
    
    for (iter in 1:max.iter) {
      
      eta <- X %*% beta0
      w <- mu <- c( exp(eta) )
      z <- eta + (y-mu)/w
      
      tilde.X <- X * sqrt(w) # = W^(1/2)X
      tilde.z <- z * sqrt(w)
      
      #stnadardized 
      bar.tilde.X <- apply(tilde.X, 2, mean)
      s.tilde.X <- sqrt(apply(tilde.X, 2, var) * (n-1)/n)
      str.tilde.X <- t((t(X) - bar.tilde.X)/s.tilde.X)
      
      beta.work <- beta0 * s.tilde.X
      r <- tilde.z - str.tilde.X %*% beta.work
      
      
      #CD
      updated.beta.work <- beta.work
      
      for (j in 1:p) {
        temp <- c( crossprod( str.tilde.X[,j] , r ) /n + beta.work[j])
        numerator <- Soft(temp, lambda[la]*alpha )
        denominator <- 1 + lambda[la]*(1-alpha) 
        
        updated.beta.work[j] <- numerator / denominator
        r <- r-(updated.beta.work[j] - beta.work[j]) * str.tilde.X[,j]
        
      }
      new.beta <- updated.beta.work / s.tilde.X
      
      delta <- max(abs(new.beta - beta0))
      if (delta < eps ) break
      
      beta0 <- new.beta
    }
    
    if (iter == max.iter) warning("Algorithm may not be converged")
    
    beta.enet <- new.beta
    beta.enet.matrix[la,] <- beta.enet
    
    
  }
  
  
  return (beta.enet.matrix)
}

