#2019020314 통계학과 이동규 codes

#########################################################################################

#1

lum <- function (x,y, init = 0 ,a=1, c=1 , lambda=1, eps = 1.0e-5 , max.iter = 100, int.num = 10 ) {
  
  # x: predictor
  # y: binary response with {-1,1} coding
  # a, c: constants in LUM loss
  # lambda: regularization parameter
  
  if (class(x) != "numeric" ) stop ("x should be a vector")
  if (class(y) != "numeric" ) stop ("y should be a vector")
  if (length(unique(y)) != 2 ) stop ("y should be a binary")
  
  n=length(x)
  V <- function(u) (1 - u) * (u < c / (1+c)) + (1/(1+c) * (a/((1+c)*u-c+a))^a ) * (u >= c/(1+c)) 
  
  alpha1 <- beta1 <- init
  
  V_alpha <- function(alpha1) sum( V (y * (alpha1 + beta1 * x )))
  V_beta <- function(beta1) 1/n * sum( V (y * (alpha1 + beta1 * x ))) + lambda/2 * beta1^2
  
  warn.index <- 1
  
  while(1) {
    
    alpha2 <- optimize(V_alpha , interval = c(-10^int.num, 10^int.num) )$minimum #int.num = interval number
    beta2 <- optimize(V_beta ,interval=c(-10^int.num, 10^int.num))$minimum
    
    delta1 <- max(abs(alpha2 - alpha1))
    delta2 <- max(abs(beta2 - beta1))
    
    if (delta1 < eps & delta2 < eps) break
    
    alpha1 <- alpha2
    beta1 <- beta2
    
    #warning  
    warn.index = warn.index+1
    if ( warn.index > max.iter ) stop("Algorithm is not converged!")
    
  }
  
  alpha <-alpha2
  beta <- beta2
  
  est <- c(alpha,beta) #linear LUM estimator
  return(est)
  
}

#########################################################################################

#2

wsvm <- function(x,y, pi=0.5 , C=1) {
  
  # x: predictor
  # y: binary response with {-1,1} coding
  # pi : weight parameter in (0,1)
  # C: cost parameter
  
  C <- C/2
  
  if (class(x) != "matrix" ) stop ("x should be a matrix")
  if (length(unique(y)) != 2 ) stop ("y should be a binary")
  if (pi > 1 & pi < 0) stop("Input 0 <= pi <= 1")
  
  
  n <- nrow(x) # sample size
  K <- x %*% t(x) + 1.0e-8 * diag(rep(1, n))
  lambda <- 1 / C
  w = (1-pi)*(y == 1) + pi*(y == -1 )
  
  c <- -lambda *rep(1,n)
  H <- y %*% t(y) * x %*% t(x)
  A <- t(y)
  b <- 0
  l <- 0 * rep(1,n)
  u <- w * rep(1,n)
  r <- 0
  
  
  obj <- ipop(c, H, A, b, l, u, r)
  alpha <- primal(obj)
  
  sv.id <- which( 0 + 1.0e-3 < alpha & alpha < (w - 1.0e-3))
  beta <- C * t(x) %*% (alpha*y)
  
  temp <- y[sv.id] - C*t(t(alpha*y) %*% x %*% t(x[sv.id,]))
  beta0 <- mean(temp)
  
  est <- c(beta0,beta) # beta0 for intercept, and beta for the coefficient vector
  return(est)
  
}

#########################################################################################

#3 - (c)

em <- function(y , delta , eps = 1.0e-5) {
  
  # y : observed survival time
  # delta : censoring indicator
  # .... : other factors required in EM Algorithm
  
  del <- sort(unique(delta))
  
  if (del[1] != 0  | del[2] != 1 ) stop ("dleta should be a indicator")
  
  sigma1 <- 1 # initial value
  n <- length(y)
  n_censored <- n - sum(delta)
  
  while (1) {
    
    sigma2 <- 1/n * {sum(y) + n_censored * sigma1 }
    sigma_eps <- sigma2-sigma1
    
    if (sigma_eps < eps ) break
    
    sigma1 <- sigma2
    
  }
  
  hat.sigma <- sigma2
  
  return (hat.sigma) #hat.sigma is the mle of sigma
  
}

#########################################################################################

#4 - (a)


mcmc <- function( x , y ,init = 0, n.samples = 10000 ) {
  
  # x : n*p predictor matrix
  # y : response vector in the Poisson regression
  # n.samples : number of posterior samples to be obtained.
  # ... : other factors required in MCMC
  
  
  if (class(x) != "matrix" ) stop ("x should be a matrix")
  
  n <- nrow(x)
  x <- cbind(c(rep(1,n)),x) # plus intercept term
  
  p <- ncol(x)
  step <- rep(0.1,p) 
  
  post.beta <- matrix(0,n.samples,p)
  ac.ratio <- matrix(0,n.samples,p)
  
  prior.m <- 0
  prior.s <- 1000
  
  #initialize
  post.beta[1,] <- beta <- rep(init, p)
  eta <- x %*% beta
  lambda <- exp(eta)
  
  log.like  <- sum(y * log(lambda) - lambda)
  
  iter <- 2
  
  for (iter in 1:n.samples){
    beta.new <- beta
    # candidate
    for (j in 1:p)
    {
      beta.new[j] <- beta[j] + rnorm(1, 0, step[j]) 
      eta.new <- x %*% beta.new
      lambda.new  <- exp(eta.new)
      
      
      # prior
      log.prior     <- dnorm(beta[j],     prior.m, prior.s, log = T)
      log.prior.new <- dnorm(beta.new[j], prior.m, prior.s, log = T)
      
      # liklihood
      log.like.new <- sum(y * log(lambda.new) - lambda.new)
      
      # ratio
      temp <- exp((log.like.new + log.prior.new) - (log.like + log.prior))
      rho <- min(1, temp)
      
      if (runif(1) < rho) {
        ac.ratio[iter,j] <- 1
        beta[j] <- beta.new[j]
        log.like  <- log.like.new
        eta <- x %*% beta
        lambda <- exp(lambda)
      }
    }
    
    post.beta[iter,] <- beta
  }
  
  samples <- post.beta
  return(samples) # n.samples * (p+1) matrix of posterior samples.
  
}

#########################################################################################

#4 - (b)
bayes.ci <- function(obj  , alpha = 0.05) {
  
  
  # obj : object from the mcmc() functions
  # alpha : confidence level
  
  n <- nrow(obj)
  p <- ncol(obj)
  posterior <- obj[-(1: (n/5)),]
  
  cr <- t(apply(posterior, 2, quantile, probs = c(alpha/2, 1-alpha/2))) # Credible Region
  
  name = NULL
  for (i in 0:(p-1)){
    init <- paste("beta",i,sep="")
    name <- c(name,init)
  }
  
  rownames(cr) <- name
  
  return(cr) # lower and upper limit of the Bayesian CI
  # 보기 좋게 제시하기위해 문제의 요구 답 형태에서 바꿨습니다.
  # (=For efficiency of visualizing, the answer form is changed.) 
  
}

#########################################################################################

#4 - (c)


boot.ci <- function (x ,y , B=500 , alpha=0.05 ) {
  
  
  # x: n*p predictor matrix
  # y: response vector in the Poisson regression
  # B: number of bootstrap repetitions
  # alpha: confidence level
  
  
  if (class(x) != "matrix" ) stop ("x should be a matrix")
  if (alpha > 1 & alpha < 0) stop("Input 0 <= alpha <= 1")
  
  n <- nrow(x)
  p <- ncol(x) +1 # 1 is intercept term
  
  # bootstrap CI
  beta.bt  <- matrix(0,B,p) # B = bootstrap sample
  
  for (b in 1:B) {
    id.bt <- sample(n, replace = T)
    x.bt <- x[id.bt,]
    y.bt <- y[id.bt]
    
    obj.mle <- glm(y.bt ~ x.bt , family = "poisson")
    beta.bt[b,] <- as.vector(obj.mle$coefficients)
    
  }
  
  # percentile CI
  CI <- t(apply(beta.bt, 2, quantile, probs = c(alpha/2, 1-alpha/2)))
  
  name = NULL
  for (i in 0:(p-1)){
    init <- paste("beta",i,sep="")
    name <- c(name,init)
  }
  
  return(CI)
  # 보기 좋게 제시하기위해 문제의 요구 답 형태에서 바꿨습니다.
  # (=For efficiency of visualizing, the answer form is changed.) 
  
}


