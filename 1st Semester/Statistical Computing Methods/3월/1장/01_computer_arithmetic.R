rm(list = ls())

# Fixed Point
# R use 32-bit integers!
as.integer(2^32)
as.integer(2^31)
as.integer(-(2^31))

# largest Integer
as.integer(2^31-1)

# smallest Integer
as.integer(-(2^31)+1)


# Floating Point
# Interesting Numbers
onep <- 1 + 2^(-52)
full <- 2 - 2^(-52)
sprintf("%a", onep)
sprintf("%a", full) 

# exactly represented integer
sprintf("%a", 2^53 - 1)
sprintf("%a", 2^53)
sprintf("%a", 2^53+1)

a = 2^53
a == (a+1)

# largest
sprintf("%a", 2^(1023))

Big <- 2^(1023) * full
sprintf("%a", Big) 

Big2 <- Big + 2^(1023 - 54) # same as Big
Big == Big2

Big3 <- Big + 2^(1023 - 53) # too large
print(Big3)

# smallest positive
sprintf("%a", 2^(-1023))
sprintf("%a", 2^(-1023 - 51))
sprintf("%a", 2^(-1023 - 52))


# Machine epsilon 
eps <- .Machine$double.eps
sprintf("%a", eps)

1 == (1 + eps)
1 == (1 + eps/2)

# exact number
2^53 - 1 # largest exactly rep. ineger
u = 2^53 # not any more
u == (u + 1)

# rounding error
# example 1
a <- (1/5 + 0.1)
b <- 0.3
a == b
sprintf("%a", c(a, b))

# example 2
x <- seq(0, 1.0, 0.1)
y <- c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)
cbind(x, y)

same <- (x == y)
data.frame(x, y, same)
data.frame(binary.x = sprintf("%a", x), binary.y = sprintf("%a", y))
