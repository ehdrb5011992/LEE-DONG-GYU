rm(list=ls()) ; gc()

library(tidyverse)
library(mice)

# data 전처리 및 결측치 해결 #


data <- read.csv('C:\\Users\\82104\\Desktop\\echocardiogram.data.csv',header=F)
colnames(data) <- c('survival','still_alive','age_at_heart_attack',
                    'pericardial_effusion','fractional_shortening',
                    'epss','lvdd','wall_motion_score','wall_motion_index',
                    'mult','name','group','alive_at_1')
  
str(data)
data <- data %>% as_tibble()
data <- data[-50,]


data[data=='?'| data==""] <- NA
data <- data %>% droplevels() %>%
  filter(still_alive != 'NA') %>% 
  select(-mult,-name) %>% 
  mutate(survival = as.character(survival) , age_at_heart_attack =as.character(age_at_heart_attack),
         pericardial_effusion=as.character(pericardial_effusion), fractional_shortening=as.character(fractional_shortening),
         epss=as.character(epss),lvdd=as.character(lvdd), wall_motion_score=as.character(wall_motion_score),
         wall_motion_index=as.character(wall_motion_index)) %>% 
  mutate(survival = as.numeric(survival) , age_at_heart_attack =as.numeric(age_at_heart_attack),
         pericardial_effusion=as.factor(pericardial_effusion), fractional_shortening=as.numeric(fractional_shortening),
         epss=as.numeric(epss),lvdd=as.numeric(lvdd), wall_motion_score=as.numeric(wall_motion_score),
         wall_motion_index=as.numeric(wall_motion_index)) 
str(data)


imp_mice <- mice(data,seed=20191112,m=10)

imp_mice$imp$age_at_heart_attack
imp_mice$imp$survival

pattern <- md.pattern(data,rotate.names=T)
colnames(pattern)[ncol(pattern)] <- '# miss'
rownames(pattern)[nrow(pattern)] <- '# miss'
pattern

stripplot(imp_mice,pch=21,cex=1.2,col=c('blue','red'))


#################################### essential function ####################################

# min max scaling
minmax_norm <- function(x) { 
  ifelse(is.na(x) , NULL , x )
  
  ( x - min(x) ) / ( max(x) - min(x) )
}
data2_2$epss <- minmax_norm(data2_2$epss)

# imputation mean
list_mean <- function(x){  
  y = 0
  for (i in 1:10) {
    y <- y +x[[i]]
  }
  result <- y/10
  return(result)
  
}

# imputation cateogry most frequency
list_most_freq <- function(x){
  y <- result <- 0
  
  for (i in 1:10){
    x[[i]]<- as.character(x[[i]]) ; x[[i]]<- as.numeric(x[[i]])
    threshold <- mean(unique(x[[i]]))
    y <- y +x[[i]]
  }
  mean.y <- y/10
  result <- ifelse(mean.y == threshold , as.numeric(I(runif(1)>0.5))+floor(threshold), 
                   ifelse(mean.y > threshold , ceiling(threshold) , floor(threshold)) )
  
  return(result)
}

#############################################################################################



####################################

# 2장. 집단의비교 시작 #

####################################
## ch1. 모평균비교 ##
### 2-1-1 정규성검정 ###
data2_1 <- data %>% 
  select(group,age_at_heart_attack) %>% 
  as.data.frame()
#group1 <- data2_1[data2_1$group==1,2] # no imputation : length 24 (subtract to NA)
#group2 <- data2_1[data2_1$group==2,2] # no imputation 
impu_aaha <- with(imp_mice,age_at_heart_attack)
impu_group <- with(imp_mice,group)

group1 <- list_mean(impu_aaha$analyses)[list_most_freq(impu_group$analyses)==1] # imputation : length 25 
group2 <- list_mean(impu_aaha$analyses)[list_most_freq(impu_group$analyses)==2]
qqnorm(group1,main='group1') ; qqline(group1) 
qqnorm(group2,main='group2') ; qqline(group2) 

### 2-1-2 상관분석 ###
set.seed(1112)
cor.test(group1,group2[sample(29)])

### 2-1-3 모분산비교 ###
fit2_1_3 <- with(imp_mice, var.test(as.formula(age_at_heart_attack~group))) 
summary(fit2_1_3)
### 2-1-4 모평균 비교 ###
fit2_1_4 <- with(imp_mice, t.test(as.formula(age_at_heart_attack~group), var.equal = TRUE, conf.level = 0.95) )
summary(fit2_1_4)



## ch2. 짝지은표본 비교 ##
### 2-2-1 정규성검정 ###
data2_2 <- data %>% 
  select(fractional_shortening ,epss) %>% 
  as.data.frame()
impu_fs <- with(imp_mice,fractional_shortening)
impu_epss <- with(imp_mice, minmax_norm(epss) )

mean_impu_fs <- list_mean(impu_fs$analyses)   
mean_impu_epss <- 0.3*( 1 - list_mean(impu_epss$analyses)) # This is because the epss variable differs from the fs variable 
                                                           # in numerical attributes.

d <- mean_impu_fs - mean_impu_epss
qqnorm(d,main='fractional_shortening') ; qqline(d)

### 2-2-2 대응표본 t-test ###
fit2_2_2 <- t.test(d)
fit2_2_2

####################################

# 3장. 분할표의 분석 시작 #

####################################

## ch1. 연관성의 측도 ##
data3_1 <- data %>% 
  select(group,alive_at_1) %>% 
  as.data.frame()

impu_alive_1 <- with(imp_mice,alive_at_1)
impu_group <- with(imp_mice, group)

mean_impu_alive_1 <- list_most_freq(impu_alive_1$analyses)
mean_impu_group <- list_most_freq(impu_group$analyses)

alive_1_factor <- as.factor(mean_impu_alive_1)
group_factor <- as.factor(mean_impu_group)

freq <- table(group_factor,alive_1_factor)

### 3-1-1 RR ###
library(epitools)
rr <- epitab(freq,method="riskratio",rev="b") 
rr$tab
### 3-2-2 OR ###
or <- epitab(freq,method = "oddsratio" , rev="b")
or$tab


## ch2. 연관성의 검증 ##
### 3-2-1. chisq ###
set.seed(1115)
data3_2_1 <- data %>% 
  select(pericardial_effusion,alive_at_1) %>% 
  as.data.frame()

impu_chisq_tables <- with(imp_mice,table(alive_at_1,pericardial_effusion))
chisq_p_val <- rep(0,10)
chisq_statistics <- rep(0,10)
for (i in 1:10){
  p_val[i] <- chisq.test(impu_chisq_tables$analyses[[i]])$p.value
  chisq_statistics[i] <- chisq.test(impu_chisq_tables$analyses[[i]])$statistic
}
chi <- rbind(p_val,chisq_statistics) ; rownames(chi) <- c("pval","statistics") ; colnames(chi) <- 1:10
chi
# 순서통계량에 근거한 유의확률의 결합
ifelse(I( min(p_val) < 1-(1-0.05)^(1/10) ),
       paste("p-은",round(min(p_val),5),"이므로 순서통계량에 근거한 방법으로 H_0를 기각합니다.") , 
       paste("p-은",round(min(p_val),5),"이므로 순서통계량에 근거한 방법으로 H_0를 받아드립니다.")) 

### 3-2-2. exact test ###
set.seed(1115)
data3_2_2 <- data %>% 
  select(alive_at_1,pericardial_effusion) %>% 
  filter(!is.na(alive_at_1)) %>% 
  sample_n(20) %>% 
  as.data.frame()

drop_exact_table <- table(data3_2)
fisher.test(drop_exact_table)

### 3-2-3 CMH test ###
set.seed(1115)
data3_2_3 <-  data %>% 
  select(alive_at_1,pericardial_effusion,group) %>% 
  as.data.frame()

impu_cmh_tables <- with(imp_mice,table(alive_at_1,pericardial_effusion,group))
cmh_p_val <- rep(0,10)
cmh_statistics <- rep(0,10)
for (i in 1:10){
  cmh_p_val[i] <- mantelhaen.test(impu_cmh_tables$analyses[[i]],correct=F)$p.value
  cmh_statistics[i] <- mantelhaen.test(impu_cmh_tables$analyses[[i]],correct=F)$statistic
}
cmh <- rbind(p_val,chisq_statistics) ; rownames(cmh) <- c("pval","statistics") ; colnames(cmh) <- 1:10
cmh
# 순서통계량에 근거한 유의확률의 결합
ifelse(I( min(p_val) < 1-(1-0.05)^(1/10) ),
       paste("p-은",round(min(p_val),5),"이므로 순서통계량에 근거한 방법으로 H_0를 기각합니다.") , 
       paste("p-은",round(min(p_val),5),"이므로 순서통계량에 근거한 방법으로 H_0를 받아드립니다.")) 


####################################

# 4장. 집단의비교 시작 #

####################################
set.seed(1117)
## ch1. 로짓모형 ##

impu_pe <- with(imp_mice,pericardial_effusion)
impu_fs <- with(imp_mice,fractional_shortening)
impu_lvdd
impu_wmi <- with(imp_mice,wall_motion_index)
impu_group <- with(imp_mice,group)
impu_aaha <- with(imp_mice,age_at_heart_attack)
impu_aa1 <- with(imp_mice,alive_at_1)

impu_trt_pe <-  list_most_freq(impu_pe$analyses) #범주형
impu_trt_fs <- list_mean(impu_fs$analyses) #연속형
impu_trt_lvdd <- list_mean(impu_lvdd$analyses) #연속형
impu_trt_wmi <- list_mean(impu_wmi$analyses) #연속형
impu_trt_group <- list_most_freq(impu_group$analyses) #범주형
impu_trt_aaha <- list_mean(impu_aaha$analyses) #연속형
impu_trt_aa1 <- list_most_freq(impu_aa1$analyses) #범주형

data4_1 <- as.data.frame(cbind(impu_trt_pe,impu_trt_fs,impu_trt_lvdd,impu_trt_wmi,impu_trt_group,impu_trt_aaha, impu_trt_aa1 ))

### 이항반응변수 ###
logistic <- glm(impu_trt_aa1 ~ as.factor(impu_trt_pe)+impu_trt_fs+impu_trt_lvdd+impu_trt_wmi+
                  as.factor(impu_trt_group)+impu_trt_aaha,family='binomial' , data=data4_1)
null_logistic <- glm(impu_trt_aa1 ~ 1 , family='binomial' , data=data4_1 )

library(lmtest)
library(MASS)

step_logistic <- logistic %>% stepAIC(trace=F)
lrtest(null_logistic,step_logistic)

summary(step_logistic)



### 다항반응변수(순서형) ###

data4_1_2 <- data4_1 %>% 
  as_tibble() %>% 
  mutate(impu_trt_aaha = ifelse(impu_trt_aaha >=70, 3,ifelse(impu_trt_aaha <50,1,2 )) ) %>% 
  mutate(impu_trt_aaha = as.factor(impu_trt_aaha)) %>% 
  as.data.frame()

setwd("C:\\Users\\82104\\Desktop")
write.csv(data4_1_2,file="ordered_multi.csv") #이후 SAS진행



##ch2. 로그선형모형 ##

data4_2_1 <- data4_1 %>% 
  select(impu_trt_pe,impu_trt_aa1,impu_trt_group) %>% 
  as.data.frame()

setwd("C:\\Users\\82104\\Desktop")
write.csv(data4_2_1,file="loglinear.csv") #이후 SAS진행


####################################

# 5장. 공분산분석 시작 #

####################################

impu_fs <- with(imp_mice,fractional_shortening)
impu_wmi <- with(imp_mice,wall_motion_index)
impu_lvdd <- with(imp_mice,lvdd)
impu_aaha <- with(imp_mice,age_at_heart_attack)

fs <- list_mean(impu_fs$analyses) #연속형
wmi <- list_mean(impu_wmi$analyses) #연속형
lvdd <- list_mean(impu_lvdd$analyses) #연속형
aaha <- list_mean(impu_aaha$analyses) #연속형

data5<- cbind(fs,lvdd,wmi,aaha ) %>% 
  as_tibble() %>% 
  mutate(aaha = ifelse(aaha >=70, 3,ifelse(aaha <50,1,2 )) ) %>% 
  mutate(aaha = as.factor(aaha)) %>% 
  as.data.frame()

  

full <- lm(lvdd ~ aaha* fs + aaha * wmi  ,data=data5)
m1 <-  car::Anova(full,type=3)
m1

restricted <- lm (lvdd~ aaha + wmi + fs  ,data=data5)
m2 <- car::Anova(restricted,type=3)
m2

library(lsmeans)
aov(lvdd ~ aaha + wmi + fs , data=data5) %>% lsmeans("aaha")


####################################

# 7장. 생존분석 시작 #

####################################


## ch1. 비모수적방법 ##

data7_1 <- data %>% 
  select(survival, still_alive) %>% 
  filter(!is.na(survival)) %>% 
  as.data.frame()

### 생명표방법 ###

min(data7_1$survival);max(data7_1$survival)
# 3개월(분기)씩 나누자

data7_1_1 <- data7_1 %>% 
  mutate(time=survival %/% 3  ) %>% 
  group_by(time,still_alive) %>% 
  summarize(freq=n()) %>% 
  ungroup() %>% 
  tidyr::complete(time,still_alive) %>% 
  mutate( freq = ifelse(is.na(freq),0,freq))

library(reshape)
data7_1_1 <- cast(data7_1_1,time~still_alive)
colnames(data7_1_1) <- c("time",'death','censor')

library(KMsurv)
life_table <- with(data7_1_1,lifetab(c(time,NA),sum(c(censor,death)),censor,death))
life_table
names(life_table)

#plot
par(mfrow=c(1,2))
length(time)
with(data7_1_1,plot(time,life_table[,5],type='s',xlab='Quarter',ylab='Survival',ylim=c(0,1)))
with(data7_1_1,plot(time,life_table[,5],type='o',xlab='Quarter',ylab='Survival',ylim=c(0,1)))

dev.off()

### 카플란마이어 방법 ###


library(survival)

data7_2_2 <- data7_1 %>% 
  mutate(censor = ifelse(still_alive == 1, 0, 1)) %>% 
  dplyr::rename(time="survival") %>% 
  select(time,censor)
  
KM <- survfit(Surv(time,censor)~1 , conf.type='log-log',data=data7_2_2)
summary(KM)

quantile(KM,probs=c(0.25,0.5,0.75),conf.int=T)

plot(KM,xlab='Monthr',ylab='Survival',mark.time=T)
legend(0.3,0.3,c("KM","95% CI"),lty=c(1,2))


## ch2. 준모수적 방법 ##

impu_aaha <- with(imp_mice,age_at_heart_attack)
impu_pe <- with(imp_mice,pericardial_effusion)
impu_fs <- with(imp_mice,fractional_shortening)
impu_lvdd <- with(imp_mice,lvdd)
impu_wmi <- with(imp_mice,wall_motion_index)
impu_group <- with(imp_mice,group)


aaha <- list_mean(impu_aaha$analyses) #연속형
pe <-  as.factor(list_most_freq(impu_pe$analyses)) #범주형
fs <- list_mean(impu_fs$analyses) #연속형
lvdd <- list_mean(impu_lvdd$analyses) #연속형
wmi <- list_mean(impu_wmi$analyses) #연속형
group <- as.factor(list_most_freq(impu_group$analyses)) #범주형

temp_data <- data.frame(survival=data$survival, still_alive=data$still_alive,aaha,pe,fs,lvdd,wmi,group)


data7_2 <- temp_data %>% 
  filter(!is.na(survival)) %>% 
  mutate(time=survival, censor= as.factor(ifelse(still_alive==1,0,1)),group=as.factor(ifelse(group==2,1,0))) %>% 
  select(time,censor,aaha,pe,fs,lvdd,wmi,group) %>% 
  filter(time>30) 

setwd("C:\\Users\\82104\\Desktop")
write.csv(data7_2,file="cox_regression.csv") #이후 SAS진행




####################################
# 끝. #
####################################


