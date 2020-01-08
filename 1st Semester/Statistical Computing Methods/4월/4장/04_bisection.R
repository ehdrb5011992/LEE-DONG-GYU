bisection <- function(f, a, b, maxiter = 100, eps = 1.0e-5) {
  if (f(a) * f(b) > 0) stop("f(a) and f(b) must have different signs")
  
  u <- b
  l <- a
  iter <- 0
  while (iter < maxiter) {
    m <- (u + l)/2
    if (f(u) * f(m) < 0) {
      l <- m
    } else if (f(l) * f(m) < 0) {
      u <- m
    } else {
      break
    }
    if (abs(l - u) < eps) break
    iter <- iter + 1
  }
  if (iter == maxiter) warning("maximum iteration reached!")
  obj <- list(sol = m, iteration = iter)
return(obj)
}

f <- function(x) cos(x) - x
a <- -10
b <- 10

obj <- bisection(f, a, b)
print(obj)
print(f(obj$sol))

