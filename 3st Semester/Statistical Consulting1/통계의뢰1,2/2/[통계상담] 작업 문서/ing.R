# https://every-day-life.tistory.com/29 참ㄱ

library(tidyverse)
library(survival)
library(survminer)
library(randomForestSRC)
library(clinfun)
# 1. data 가볍게 전처리 ########################################
x= read.csv('C:\\Users\\82104\\Desktop\\600.csv',header=T)
head(x)
name <- colnames(x)
colnames(x) <- c("a","b","c","d","e","f","g","h","i","j",
                 "k","l","m","n","o","p","q","r","s","t","u","v","w")

# a 나이 / b 성별 (1:남자, 2:여자) / c 결혼 (1:기혼, 2:미혼) /
# d 종교 (1:유, 2:무) / e 학위 (1:3년제이하, 2:4년제이상) /
# f 부서 (1:내과, 2:외과) / g 이직계획유무 (1:유, 2:무) /
# h 건강상태 (1:좋음, 2:나쁨)/ i 근무지역 (1:수도권, 2:지방) /
# j 공감_tot / k 공감_com / l 공감_sen / m 공감_ins /
# n 이직여부(안씀) / o 이직,퇴직여부(0:censoring 1:event) /
# p 임상경력(일)(안씀) / q 최종임상경력(일)(안씀) / 
# r 최종임상경력(월) (생존시간) / s 최종임상경력(년) 안씀 /
# t 병동경력(일)(안씀) / u 년차_최종 (1:3년차이하, 2:3~6 , 3:6년차 초과) /
# v 연차별시작문항응답(안씀) / w 연차별최종문항응답(안씀)

x$b <- as.factor(x$b)
x$c <- as.factor(x$c)
x$d <- as.factor(x$d)
x$e <- as.factor(x$e)
x$f <- as.factor(x$f)
x$g <- as.factor(x$g)
x$h <- as.factor(x$h)
x$i <- as.factor(x$i)
x$u <- as.factor(x$u)


# 나이 그룹 2개로 나누기 
# 20대 : 1 , 30대이상 : 2
x$age <- 2
for (i in 1:nrow(x)){
  if (x$a[i] < 30) {
    x$age[i] <- 1
  }
}
x$age <- as.factor(x$age)


# 공감역량 4개 분포 (전부 정규분포)
with(x,hist(k)) # communication 
with(x,hist(l)) # sensitivity
with(x,hist(m)) # inspiration
with(x,hist(j)) # total

# 3개로 그룹화. 낮음, 보통, 높음
# 이 범주형 변수화는 왠지 효과는 없는 듯 하다.
for (i in 1:nrow(x)){
  if (x$k[i]<3){
    x$trt_k[i] <- 1
  } else if(x$k[i]<4){
    x$trt_k[i] <- 2
  } else{
    x$trt_k[i] <- 3
  }
  if (x$l[i]<3){
    x$trt_l[i] <- 1
  } else if(x$l[i]<4){
    x$trt_l[i] <- 2
  } else{
    x$trt_l[i] <- 3
  }  
  if (x$m[i]<3){
    x$trt_m[i] <- 1
  } else if(x$m[i]<4){
    x$trt_m[i] <- 2
  } else{
    x$trt_m[i] <- 3
  }
  if (x$j[i]<3){
    x$trt_j[i] <- 1
  } else if(x$k[i]<4){
    x$trt_j[i] <- 2
  } else{
    x$trt_j[i] <- 3
  }
}

x$trt_k <- as.factor(x$trt_k)
x$trt_l <- as.factor(x$trt_l)
x$trt_m <- as.factor(x$trt_m)
x$trt_j <- as.factor(x$trt_j)



# 1. total
x1 <- x

# 2. 3년차 미만
x2 <- x[x$u == 1,]

# 3. 3년차 이상 6년차 미만
x3 <- x[x$u == 2,]

# 4. 6년차 이상
x4 <- x[x$u == 3,]



################################################################


# class가 2개라면, 1 대비 2가 exp(회귀계수)만큼 위험함. 
# (ex.변수 c 1:기혼 2:미혼 -> 기혼대비 미혼이 exp(0.36514) = 1.44배 위험함. )
# 다르게 해석하고 싶다면 x$c <- relevel(x$c, ref = "patchOnly") 후, 다시 돌리면 됨.
fit.coxph <- coxph(Surv(r,o)~a+c+e+g+h+k+l+m, data=x,ties='breslow')
summary(fit.coxph)
with(x,hist(r))
with(x,table(o,r))

with(x,hist(a)) # 나이의 분포 

# matritngale residual (연속형변수 처리방법)
# 그룹을 나이로 보고, 위험도를 살펴봄. 여기서 연속형변수를 어떻게 범주화시킬지 고려 가능
# 지금과 같은 경우, 20대 / 20대 아닌사람들 이렇게 나누기 합당해보임.
m1 <- coxph(Surv(r,o)~1,data = x)
x$resid <- residuals(m1, type = "martingale")
x %>% ggplot(aes(a,resid))+
  geom_point()+
  geom_smooth()+
  theme_classic()



# 나이 그룹나눠서 해본것 (안좋음)
fit.coxph <- coxph(Surv(r,o)~age+c+e+g+h+k+l+m, data=x,ties='breslow')


# 그룹화 해서 해본것 / trt_k,trt_l,trt_m 적용
# x1,x2,x3,x4 dataset 
# x1
fit.coxph <- coxph(Surv(r,o)~a+c+e+g+h+k+l+m, data=x1,ties='breslow') # age ,trt조합 다 안좋음.
summary(fit.coxph) 
# x2
fit.coxph <- coxph(Surv(r,o)~g+k+l+m, data=x2,ties='breslow') # age ,trt조합 다 안좋음.
summary(fit.coxph) 
# x3
fit.coxph <- coxph(Surv(r,o)~b+g+k+l+m, data=x3,ties='breslow') # age ,trt조합 오히려 더 안좋음.
summary(fit.coxph) 
# x4
fit.coxph <- coxph(Surv(r,o)~g+h+k+l+m, data=x3,ties='breslow') # age ,trt조합 오히려 더 안좋음.
summary(fit.coxph) 






###################### 비례위험 확인 part ######################

# 1. Kaplan curve 를 이용한 비례위험 확인
m1 <- survfit(Surv(r,o)~b,data=x)
plot(m1, col=c("red","blue"))

#2. Scheonfeld method 통계량 & plot 확인 (기울기=0 검정) 
# 변수마다 다 봐야함. 비례위험을 만족하는지 안하는지 
# a,b,c,d,e,f,g,h,i (나이, 성별, 결혼, 종교,학위, 부서, 이직계획유무, 건강상태, 근무지)
# 굉장히 볼게 많다. 그룹 3개별, 각 변수별 다 살펴봐야함 (비례위험을 만족하지 않은 변수에 대해)
m2 <- coxph(Surv(r,o)~c,data = x)
summary(m2)
cox.zph(m2)
plot(cox.zph(m2))
abline(h=0, col="red", lty=2)
abline(h=0.7055,col="blue")


#3. time dependent covariate 투입
m5 <- coxph(Surv(r,o)~c+tt(c),data=x) # 유의하기 때문에 , tt(c) 항목을 남겨놓으면 됨.
m5

# Scheonfeld residual을 이용한 방법에 비해 신뢰도가 떨어지며, 
# 좀 더 정확하게는 time, log(time), rank(time) 등, time을 변형한 수치도 투입해서 확인해야 한다는 점임.
# (위의 예제는 time transfer function(=tt)에 아무 옵션을 주지 않았기 때문에 rank(time)이 기본치이다.)   

################################################################






################# non proportional hazard 교정 #################

# 1. step function (연속형일때) - 자르는거임.
m2 <- coxph(Surv(r,o)~a,data = x)
summary(m2)
cox.zph(m2)
plot(cox.zph(m2))
abline(h=0, col="red", lty=2)
abline(h=-0.1053,col="blue")

sub_x <- survSplit(Surv(r,o)~.,data=x,cut=c(90,100),episode = "tgroup")
head(x)
head(sub_x)
table(sub_x$tgroup)



m1 <- coxph(Surv(r,o)~a,data=x)
m1
cox.zph(m1)

m2 <- coxph(Surv(time = tstart,time2 = r,event = o)~a:strata(tgroup),data=sub_x)
m2
cox.zph(m2)


#2. time dependent covariate 투입 (연속형변수)
m1 <- coxph(Surv(r,o)~a+tt(a),data=x,tt=function(x,t, ...) x*t)
summary(m1)

m2 <- coxph(Surv(r,o)~a+tt(a),data=x,tt=function(x,t, ...) x*log(t))


#3. Stratified Cox 비례위험모형 (이게 아마 내가해야할 모형 , 범주형변수)

fit.coxph <- coxph(Surv(r,o) ~ a+strata(c)+e+g+h+k+l+m, data=x)
summary(fit.coxph) 

fit.coxph1 <- coxph(Surv(r, o) ~ a+e+g+h+k+l+m, data=x[x$c== 1,])
summary(fit.coxph1)
fit.coxph2 <- coxph(Surv(r, o) ~ a+e+g+h+k+l+m, data=x[x$c== 2,])
summary(fit.coxph2)


m2 <- coxph(Surv(r,o)~a+strata(c),data = x)
cox.zph(m2)
plot(cox.zph(m2))





################################################################################
################################################################################
################################################################################

# 분석 start


?plot.coxph

# 1. 전체대상  
# c(결혼) 변수를 층화(교호작용 고려)하여, 분석함.
# 이때, 변수는 univariate로 survival에 p<0.2 인 유의한 변수를 택했으며,
# 이 변수들 중, 비례위험 가정을 만족하지 않는 변수는 cox.zph함수를 통해 c(결혼) 만 있었기에, 층화시킴.(범주형) 
# 그리고 결혼층은 다른 변수들과 교호작용이 있는 것으로 판단, 모형을 교호작용텀으로 적용하였음.
# cf) 나이는 연속형 변수고, 시간에 따라 변하지만, 고정된 공변량으로 처리한다고 함. 따라서 열외.

# fit.coxph <- coxph(Surv(r, o) ~ c, data = x1) ... fit.coxph <- coxph(Surv(r, o) ~ i, data = x1)
# summary(fit.coxph) ; cox.zph(fit.coxph)

fit.coxph <- coxph(Surv(r, o) ~ c, data = x1,ties='breslow')
summary(fit.coxph)
cox.zph(fit.coxph)


plot(cox.zph(fit.coxph) , ylab = 'Beta(t) for Marriage', xlab='Time (month)' , main = 'Mariage: Schoenfeld Individual Test (p= 0.001)')
abline(h=0, col="red", lty=2)
abline(h=0.7055,col="blue")
?plot


# 아래는 교호작용x, 단순 층화
fit.coxph0 <- coxph(Surv(r, o) ~ a+strata(c)+e+g+h+k+l+m, data=x1)
summary(fit.coxph0)



### 교호작용 선택모형 검정.
strata.mariage <- coxph(Surv(r, o) ~ a+strata(c)+e+g+h+k+l+m, data=x1)
strata.mariage
interaction.mariage <- coxph(Surv(r, o)  ~ (a+e+g+h+k+l+m)*c - c + strata(c), data=x1)
interaction.mariage
anova(interaction.mariage, strata.mariage) # 유의하므로, 교호작용 모델 선택.

# 아래는 교호작용 고려 (이게 결과제시로 쓸 것.)
# 단, 여기서 교호작용이 있기 때문에, 추정된 계수값은 bias가 존재. 

fit.coxph1 <- coxph(Surv(r, o) ~ a+e+g+h+k+l+m, data=x1[x1$c== 1,])
summary(fit.coxph1)
fit.coxph2 <- coxph(Surv(r, o) ~ a+e+g+h+k+l+m, data=x1[x1$c== 2,])
summary(fit.coxph2)

# 위 두개를 전부 해석해야함.

new_df <- with(x1[x1$c== 1,],
               data.frame(a = rep(mean(a, na.rm = TRUE), 8), 
                          e = c(1,1,1,1,0,0,0,0)+1,
                          g = c(1,1,0,0,1,1,0,0)+1,
                          h = c(1,0,1,0,1,0,1,0)+1,
                          k = rep(mean(k, na.rm = TRUE), 8),
                          l = rep(mean(l, na.rm = TRUE), 8),
                          m = rep(mean(m, na.rm = TRUE), 8)
                          
               )
)
new_df

####### cox regression 새로운 값이 주어졌을때 예측 그래프. #######
#### survival graph fit.coxph1
# 공감_ins 점수 1(strata=1),3(strata=2),5(strata=3) 별 그림임. 
# 나이 : 평균, 공감_com : 평균 , 공감_sen : 평균,  e학위 : 4년제이상 ,  g 이직계획유무: 없음, h 건강상태: 나쁨
# example
new_df <- with(x1[x1$c== 1,],
               data.frame(a = rep(mean(a, na.rm = TRUE), 3), 
                          e = as.factor(c(1,1,1)+1),
                          g = as.factor(c(1,1,1)+1),
                          h = as.factor(c(1,1,1)+1),
                          k = rep(mean(k, na.rm = TRUE), 3),
                          l = rep(mean(l, na.rm = TRUE), 3),
                          m = c(1,3,5)
                          
               )
)
new_df
# Survival curves with new data
library(survminer)
fit <- survfit(fit.coxph1, newdata = new_df)
ggsurvplot(fit, conf.int = F, 
           censor = FALSE, surv.median.line = "hv",data=x1[x1$c== 1,],legend.title="",
           legend.labs=c("Com_ins_1","Com_ins_3","Com_ins_5"),linetype=c(1,2,3))


#### survival graph fit.coxph2
# example
# 공감_sen 점수 1(strata=1),3(strata=2),5(strata=3) 별 그림임. 
# 나이 : 평균, 공감_com : 평균 , 공감_ins : 평균,  e학위 : 4년제이상 ,  g 이직계획유무: 없음, h 건강상태: 나쁨
new_df <- with(x1[x1$c== 2,],
               data.frame(a = rep(mean(a, na.rm = TRUE), 3), 
                          e = as.factor(c(1,1,1)+1),
                          g = as.factor(c(1,1,1)+1),
                          h = as.factor(c(1,1,1)+1),
                          k = rep(mean(k, na.rm = TRUE), 3),
                          l = c(1,3,5),
                          m = rep(mean(l, na.rm = TRUE), 3)
                          
               )
)
new_df
# Survival curves with new data
library(survminer)
fit <- survfit(fit.coxph2, newdata = new_df)
ggsurvplot(fit, conf.int = F, 
           censor = FALSE, surv.median.line = "hv",data=x1[x1$c== 2,],legend.title="",
           legend.labs=c("Com_sen_1","Com_sen_3","Com_sen_5"),linetype=c(1,2,3))

#################################################################################







# 2. 3년차이하  ******
# 변수는 univariate로 survival에 p<0.2 인 유의한 변수를 택함. 
# 변수 g는 비례위험가정을 만족하나, 모형이 적합하지 않음.


# fit.coxph <- coxph(Surv(r, o) ~ a, data = x2, ties='breslow') ... fit.coxph <- coxph(Surv(r, o) ~ i, data = x2, ties='breslow')
# summary(fit.coxph) ; cox.zph(fit.coxph)

# 비례위험 체크 (g변수)


fit.coxph <- coxph(Surv(r, o) ~ g, data = x2) ;summary(fit.coxph) ; cox.zph(fit.coxph)

plot(cox.zph(fit.coxph))
abline(h=0, col="red", lty=2)
abline(h=-0.5819,col="blue")

fit.coxph0 <- coxph(Surv(r, o) ~ g+k+l+m, data=x2)
summary(fit.coxph0)



# 3. 3년차초과 6년차 이하 
# 변수는 univariate로 survival에 p<0.2 인 유의한 변수를 택함. 
# cox모형 그냥 다 만족하므로, 그냥 쓰면 됨.

# fit.coxph <- coxph(Surv(r, o) ~ a, data = x3, ties='breslow') ... fit.coxph <- coxph(Surv(r, o) ~ i, data = x3, ties='breslow')
# summary(fit.coxph) ; cox.zph(fit.coxph)

# 비례위험 체크 (b,g변수)
fit.coxph <- coxph(Surv(r, o) ~ b, data = x3) ;summary(fit.coxph) ; cox.zph(fit.coxph)
fit.coxph <- coxph(Surv(r, o) ~ g, data = x3) ;summary(fit.coxph) ; cox.zph(fit.coxph)


# 모형도 적합, k,l,m 유의한거 확인.
fit.coxph0 <- coxph(Surv(r, o) ~ b+g+k+l+m, data=x3,ties='breslow')
summary(fit.coxph0)




# 4. 6년차 초과   ******
# 변수는 univariate로 survival에 p<0.2 인 유의한 변수를 택함. 
# fit.coxph <- coxph(Surv(r, o) ~ a, data = x4, ties='breslow') ... fit.coxph <- coxph(Surv(r, o) ~ i, data = x4, ties='breslow')
# summary(fit.coxph) ; cox.zph(fit.coxph)

coxph(Surv(r, o) ~ i, data = x4, ties='breslow')


fit.coxph <- coxph(Surv(r, o) ~ g, data = x4) ;summary(fit.coxph) ; cox.zph(fit.coxph)


fit.coxph0 <- coxph(Surv(r, o) ~ g+k+l+m, data=x4)
summary(fit.coxph0)
