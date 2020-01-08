rm(list=ls())
gc()

#install.packages("RConics") # related to adjoint matrix
#library(RConics)

X <-cbind(rep(1,7), -3:3 , (0:6)^2 ) 
y <- c(1:3,seq(5,11,2))

# 1. Householder transformation 
# It defines diffrently between my u vector and slide's u vector
# slide : u=x+se1 & my : u=x-se1 -> leads to diffrent orthnormal matrix U (almost equal) 


Ht <- function(X) {
  
  n <- nrow(X)
  p <- ncol(X)
  U <- diag(n)
  New_X <- X
  
  for (i in 1:p) {
    
    
    x <- New_X[i:n,i] #x의 첫째줄. 
    s <- sqrt(c(crossprod(x)))
    e1 <- c(1,rep(0,n-i))
    u <- x - s*e1 #u벡터
    
    d <- 2/c(crossprod(u))
    sub_U <- diag(n-i+1) - d*outer(u,u)
    
    U2 <- diag(n)
    U2[i:n,i:n] <- sub_U
    
    U <- U2 %*% U
    New_X <- U %*% X
  }
  
  R <- New_X[1:p,1:p] # 관심있는 upper만 뽑음.
  ans <- list(U=U, R=R)
  
  return(ans)
  
}


my_lm_Ht <- function(X,y){ 
  
  Xy<-cbind(X,y)
  n <- nrow(X)
  p <- ncol(X)
  U <- Ht(X)$U
  R <- Ht(X)$R
  
  z1 <- (U %*% y)[1:p]
  z2 <- (U %*% y)[(p+1):n]
  
  #1. beta
  b <- backsolve(R , z1) # b = beta
    
  #2. predicted value
  
  yhat <- t(U) %*% c(z1 , rep(0,n-p))
  
  #3. residuals
  ehat <- t(U) %*% c(rep(0, p), z2)
  
  #4. SSR & MSR
  yhat_ybar <- c(z1,rep(0,n-p)) - U %*% rep(mean(y),n)
  SSR <- c(crossprod(yhat_ybar))
  n_SSR <- p-1
  MSR <- SSR / n_SSR
  
  #5. SSE $ MSE
  SSE <- c(crossprod(z2,z2))
  n_SSE <- n-p
  MSE <- SSE / n_SSE
  
  # 4&5.
  ANOVA <- matrix(c(n_SSR,n_SSE,SSR,SSE,MSR,MSE),2,3)
  colnames(ANOVA)<-c("Df","Sum sq","Mean sq")
  rownames(ANOVA) <-c("Model","Residuals")
  
  #6. coef(beta) standard errors in GM model

  R_inv <- 1/prod(diag(R)) * adjoint(R) #R is upper triangular matrix
  beta_cov_matrix <- R_inv %*% t(R_inv) * MSE # covariance matrix
  var_b <- sqrt(diag(beta_cov_matrix))
  
  
  ans <- list(b=b , beta_cov_matrix=beta_cov_matrix, 
              var_b=var_b, yhat=yhat , ehat=ehat, ANOVA=ANOVA)
  
  return(ans)
}



kk<- lm(y~X[,2] + X[,3]) # 과 비교하는거임.
summary(kk)
kk$fitted.values
anova(kk)



#2. Givens transformation

Gt <- function(X) {
  
  n <- nrow(X)
  p <- ncol(X)
  U2 <- diag(n)
  New_X <- X
  
  for (j in 2:n) {
    for (i in 1:min(j-1,p)) {
      
     U <- diag(n)
     s_ij <- sqrt(New_X[i,i]^2 + New_X[j,i]^2)
     U[i,i] <-New_X[i,i]/s_ij ; U[i,j] <-New_X[j,i]/s_ij
     U[j,i] <--New_X[j,i]/s_ij ;U[j,j] <-New_X[i,i]/s_ij
     
     New_X <- U %*% New_X
     U2 <- U %*% U2 
      
    }
  }
  
  R <- New_X[1:p,1:p] # 관심있는 upper만 뽑음.
  ans <- list(U=U2, R=R)
  
  return(ans)
  
  
  
}


my_lm_Gt <- function(X,y){ #my_lm_HT 와 다른점은 Ht함수 대신 Gt함수를 불러온 점 하나뿐이다. 2군데만 다르다.
  
  Xy<-cbind(X,y)
  n <- nrow(X)
  p <- ncol(X)
  U <- Gt(X)$U #다름1.
  R <- Gt(X)$R #다름2.
  
  z1 <- (U %*% y)[1:p]
  z2 <- (U %*% y)[(p+1):n]
  
  #1. beta
  b <- backsolve(R , z1) # b = beta
  
  #2. predicted value
  
  yhat <- t(U) %*% c(z1 , rep(0,n-p))
  
  #3. residuals
  ehat <- t(U) %*% c(rep(0, p), z2)
  
  #4. SSR & MSR
  yhat_ybar <- c(z1,rep(0,n-p)) - U %*% rep(mean(y),n)
  SSR <- c(crossprod(yhat_ybar))
  n_SSR <- p-1
  MSR <- SSR / n_SSR
  
  #5. SSE $ MSE
  SSE <- c(crossprod(z2,z2))
  n_SSE <- n-p
  MSE <- SSE / n_SSE
  
  # 4&5.
  ANOVA <- matrix(c(n_SSR,n_SSE,SSR,SSE,MSR,MSE),2,3)
  colnames(ANOVA)<-c("Df","Sum sq","Mean sq")
  rownames(ANOVA) <-c("Model","Residuals")
  
  #6. coef(beta) standard errors in GM model
  
  R_inv <- 1/prod(diag(R)) * adjoint(R) #R is upper triangular matrix
  beta_cov_matrix <- R_inv %*% t(R_inv) * MSE # covariance matrix
  var_b <- sqrt(diag(beta_cov_matrix))
  
  
  ans <- list(b=b , beta_cov_matrix=beta_cov_matrix, 
              var_b=var_b, yhat=yhat , ehat=ehat, ANOVA=ANOVA)
  
  return(ans)
}


