rm(list = ls()) # clean up memory

# response
y <- c(3.5, 5.5, 6.5, 8, 10, 12)

# predictor
X <- cbind(rep(1, 6), 1:6, (1:6)^2)

Xy <- cbind(X, y)
print(Xy)

I6 <- diag(6) # identity matrix

# HT update 
# 1st column 
u1 <- Xy[,1] # c1
ss1 <- c(crossprod(u1, u1))
s1  <- sign(u1[1]) * sqrt(ss1)
d <- 1/(ss1 + s1 * u1[1])
u1[1] <- u1[1] + s1 # u1
U1 <- I6 - outer(d*u1, u1)

Xy <- U1 %*% Xy

# 2nd column
u2 <- Xy[2:6,2] # c2
ss2 <- c(crossprod(u2, u2))
s2  <- sign(u2[1]) * sqrt(ss2)
d <- 1/(ss2 + s2 * u2[1])
u2[1] <- u2[1] + s2
U2 <- I6
U2[2:6,2:6] <- I6[2:6,2:6] - outer(d * u2, u2)

Xy <- U2 %*% Xy

# 3rd column
u3 <- Xy[3:6,3] # c2
ss3 <- c(crossprod(u3, u3))
s3  <- sign(u3[1]) * sqrt(ss3)
d <- 1/(ss3 + s3 * u3[1])
u3[1] <- u3[1] + s3
U3 <- I6
U3[3:6,3:6] <- I6[3:6,3:6] - outer(d * u3, u3)

Xy <- U3 %*% Xy

# U ( = Q transpose)
Qt <- U <- U3 %*% U2 %*% U1
print(U)


# regression coefficient
b <- backsolve(Xy[1:3, 1:3], Xy[1:3,4])

z1 <- Xy[1:3,4]
z2 <- Xy[-(1:3),4]

# predicted value
yhat <- t(Qt) %*% c(z1, rep(0, 3))

# residuals
ehat <- t(Qt) %*% c(rep(0, 3), z2)

# SSR
SSR <- crossprod(z1, z1)
print(SSR)

# SSE
SSE <- crossprod(z2, z2)
print(SSE)


# Do it again by using qr() function
qr.obj <- qr(X) # QR decomposition

# Q = U1 %*% U2 %*% U3
Q <- qr.Q(qr.obj, complete = T) 

# get z
z.qr <- qr.qty(qr.obj, y)

# fitted value
qr.qy(qr.obj, c(z.qr[1:3], rep(0, 3)))

# residual
qr.qy(qr.obj, c(rep(0, 3), z.qr[4:6]))

