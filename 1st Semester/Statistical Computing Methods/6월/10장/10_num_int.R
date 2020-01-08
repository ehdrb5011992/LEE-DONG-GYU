mid.rule <- function(a, b, ft){
 mid <- (a + b)/2
 v <- ft(mid) * (b-a)
 return(v)
}


tra.rule <- function(a, b, ft) {
  f1 <- ft(a)
  f2 <- ft(b)
  v <- (f1 + f2) * (b - a)/2
  return(v)
}

sim1.rule <- function(a, b, ft) {
  h <- (b-a)/2
  x0 <- a
  x1 <- a + h
  x2 <- b
  
  v <- h/3 * (ft(x0) + 4 * ft(x1) + ft(x2))
  return(v)
}


sim2.rule <- function(a, b, ft) {
  h <- (b-a)/3
  x0 <- a
  x1 <- a + h
  x2 <- a + 2*h
  x3 <- b
  
  v <- 3*h/8 * (ft(x0) + 3 * ft(x1) + 3 * ft(x2) + ft(x3))
  return(v)
}


my.integrate <- function(ft, a, b, method = "mid", n.grid = 1000)
{
  x.grid <- seq(a, b, length = n.grid + 1)
  if (method == "mid") {
    v <- mid.rule(x.grid[1:n.grid], x.grid[2:(n.grid+1)], ft)  
  } else if (method == "tra") {
    v <- tra.rule(x.grid[1:n.grid], x.grid[2:(n.grid+1)], ft)  
  } else if (method == "sim1") {
    v <- sim1.rule(x.grid[1:n.grid], x.grid[2:(n.grid+1)], ft)  
  } else if (method == "sim2") {
    v <- sim2.rule(x.grid[1:n.grid], x.grid[2:(n.grid+1)], ft)  
  } else stop("method should be properly specified!")
  obj <- sum(v)

  return(obj)
}


ft <- function(x) 0.2 + 25*x + 3*x^2 + 8*x^3

I <- 90.4

v1 <- my.integrate(ft, a, b, method = "mid")
v2 <- my.integrate(ft, a, b, method = "tra")
v3 <- my.integrate(ft, a, b, method = "sim1")
v4 <- my.integrate(ft, a, b, method = "sim2")

print(c(I, v1, v2, v3, v4))




