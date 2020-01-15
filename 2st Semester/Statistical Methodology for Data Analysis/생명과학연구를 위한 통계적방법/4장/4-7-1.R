rm(list=ls())
gc()
library(lmtest)


dat<-read.table('C:\\Users\\82104\\Desktop\\4_7.txt',header=T)
colnames(dat) <- c("x","y","z","count")


xyz <- glm(count ~ .^3, data = dat, family = poisson)
xy_yz_xz <- glm(count ~ .^2, data = dat, family = poisson)
summary(xy_yz_xz)
lrtest(xy_yz_xz,xyz) #lrtest확인.

yz_xz <- glm(count ~ .^2 -x:y,data=dat, family=poisson)
xy_xz <- glm(count ~ .^2 -y:z,data=dat, family=poisson)
xy_yz <- glm(count ~ .^2 -x:z,data=dat, family=poisson)
xy_z <- glm(count ~ . +x:y,data=dat, family=poisson)
yz_x <- glm(count ~ . +y:z,data=dat, family=poisson)
xz_y <- glm(count ~ . +x:z,data=dat, family=poisson)
x_y_z <- glm(count ~ . ,data=dat, family=poisson)

t0 <- lrtest(xy_yz_xz,xyz)$Chisq

t1 <- lrtest(xy_yz,xy_yz_xz)$Chisq
t2 <- lrtest(xy_xz,xy_yz_xz)$Chisq
t3 <- lrtest(yz_xz,xy_yz_xz)$Chisq

t4 <- lrtest(xy_z,xy_yz_xz)$Chisq
t5 <- lrtest(yz_x,xy_yz_xz)$Chisq
t6 <- lrtest(xz_y,xy_yz_xz)$Chisq

t7 <- lrtest(x_y_z,xy_yz_xz)$Chisq

G_2 <- as.matrix(rbind(t0,t1,t2,t3,t4,t5,t6,t7)[,2])
G_2 <- cbind( G_2 ,G_2[,1] - G_2[1,1] )
colnames(G_2) <- c("G2(M)","G2(M2|M1)")
rownames(G_2) <- c("xy_yz_xz","xy_yz","xy_xz","yz_xz","xy_z","yz_x","xz_y","x_y_z")
G_2
