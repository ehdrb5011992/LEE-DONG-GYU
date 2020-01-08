choleksy <- function(A)
{
  n <- ncol(A)
  # initalization
  Lk <- sqrt(A[1,1])
  k <- 2
  for (k in 2:n) {
    ak <- A[1:(k-1),k]
    ell.k <- forwardsolve(Lk, ak)
    ell.kk <- sqrt(A[k,k] - crossprod(ell.k, ell.k))
    Lk <- rbind(cbind(Lk, rep(0, k-1)), c(ell.k, ell.kk))
  }
  L <- Lk
  return(L)  
}
