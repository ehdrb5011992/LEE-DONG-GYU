mcmc.logit2 <- function(x, y, init, n.sample = 10000, step = rep(0.3, p)){
  n <- nrow(x)
  p <- ncol(x)
  
  post.beta <- matrix(0, n.sample, p)
  ac.ratio <- matrix(0, n.sample, p)
  
  prior.m <- 10
  prior.s <- 1000 # for vague prior
  
  # intialize
  post.beta[1,] <- beta <- init
  eta <- x %*% beta
  pi <- exp(eta)/(1 + exp(eta))
  
  log.like  <- sum(y * log(pi) +  (1 - y) * log(1 - pi))
  
  for (iter in 1:n.sample){
    beta.new <- beta
    # candidate
    for (j in 1:p)
    {
      beta.new[j] <- beta[j] + rnorm(1, 0, step[j]) 
      eta.new <- x %*% beta.new
      pi.new  <- exp(eta.new)/(1 + exp(eta.new))
      
      # prior
      log.prior     <- dnorm(beta[j],     prior.m, prior.s, log = T)
      log.prior.new <- dnorm(beta.new[j], prior.m, prior.s, log = T)
      
      # liklihood
      log.like.new <- sum(y * log(pi.new) + (1 - y) * log(1 - pi.new))
      
      # ratio
      temp <- exp((log.like.new + log.prior.new) - (log.like + log.prior))
      rho <- min(1, temp)
      
      if (runif(1) < rho) {
        ac.ratio[iter,j] <- 1
        beta[j] <- beta.new[j]
        log.like  <- log.like.new
        eta <- x %*% beta
        pi <- exp(eta)/(1 + exp(eta))
      }
    }
    
    post.beta[iter,] <- beta
  }
  
  obj <- list(posterior = post.beta, acpt.ratio = apply(ac.ratio, 2, mean))
  return(obj)
}