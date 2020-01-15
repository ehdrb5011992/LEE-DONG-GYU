# Chapter 12. Monte-Carlo simulation study 

sigma <- 0.01
grid <- expand.grid(x=seq(-2.5,2.5,0.1),y=seq(-2.5,2.5,0.1))
grid$z <- exp(-grid$x^2) + exp(-grid$y^2) + rnorm(51^2,0,sigma)
library(lattice)
wireframe(z ~ x * y, data=grid, aspect=c(1,0.5), main="Simulated Data", screen=list(z=30,x=-60),drape=T)
library(mgcv)
gam.sim <- gam(z ~ s(x)+s(y), data=grid)
gam.z <- predict(gam.sim, newdata=grid)
x11(); wireframe(gam.z ~ x * y, data=grid, aspect=c(1,0.5), main="GAM Surface", screen=list(z=30,x=-60),drape=T)

grid$z <- exp(-grid$x^2)*exp(-grid$y^2) + rnorm(51^2,0,sigma)
wireframe(z ~ x * y, data=grid, aspect=c(1,0.5), main="Simulated Data", screen=list(z=30,x=-60),drape=T)
gam.sim <- gam(z ~ s(x)+s(y), data=grid)
gam.z <- predict(gam.sim, newdata=grid)
x11(); wireframe(gam.z ~ x * y, data=grid, aspect=c(1,0.5), main="GAM Surface", screen=list(z=30,x=-60),drape=T)

loess.sim <- loess(z ~ x + y, data=grid)
loess.z <- predict(loess.sim, newdata=grid)
x11(); wireframe(loess.z ~ x * y, data=grid, aspect=c(1,0.5), main="LOESS Surface", drape=T, screen=list(z=30,x=-60))
loess.sim <- loess(z ~ x + y, span=0.5, data=grid)
loess.z <- predict(loess.sim, newdata=grid)
x11(); wireframe(loess.z ~ x * y, data=grid, aspect=c(1,0.5), main="LOESS Surface", drape=T, screen=list(z=30,x=-60))

# end
