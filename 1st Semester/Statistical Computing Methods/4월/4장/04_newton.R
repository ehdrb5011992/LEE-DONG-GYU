newton <- function(f, df, init = 0, maxiter = 100, eps = 1.0e-8)
{
  x <- init
  iter <- 0
  while (iter < maxiter) {
    new.x <- x - f(x)/df(x)
    if (abs((new.x - x)/x) < eps) break
    iter <- iter + 1
    x <- new.x
  }
  if (iter == maxiter) warning("maximum iteration reached!")
  obj <- list(sol = new.x, iteration = iter)
  return(obj)
}
  

f <- function(x) cos(x) - x
df <- function(x) -sin(x) - 1

obj <- newton(f, df, init = 1)
print(obj)
print(f(obj$sol))
