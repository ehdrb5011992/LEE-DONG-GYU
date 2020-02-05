library(ggplot2)
x=matrix(c(1,2,3,1,2,3),2,3)
x = data.frame(x) #무조건 데이터프레임만 가능.
ggplot(x,aes(X1,X2))+geom_point()


##################geom_bar##################
ggplot(iris,aes(x=Species)) +geom_bar()
#1.
ggplot(iris,aes(Species,fill=Species)) + geom_bar()
#2.테두리
ggplot(iris,aes(Species,fill=Species)) + geom_bar(color='black')
#3.테두리 두께
ggplot(iris,aes(Species,fill=Species)) + geom_bar(color='black',size=2)
#4.bar두께
ggplot(iris,aes(Species,fill=Species)) + geom_bar(width=0.2)
#5.범례제거
ggplot(iris,aes(Species,fill=Species)) + geom_bar(show.legend=F)
?geom_bar

##################geom_col##################
#각 x값에 해당하는 y를 누적합 하라는 뜻 = geom_col()
ggplot(iris,aes(Species,Sepal.Width)) + geom_col()
ggplot(iris,aes(Species,Sepal.Width,fill=Species)) + 
  geom_col(width=0.2,show.legend=F)



#################geom_line################
library(dplyr) ; str(CO2)
#dplyr에서 제공하는 pipe연산자 %>% // 맨앞에 파라미터가 오고, 뒤에 갈수록 함수를 적음.
# ctrl + shift + m 이 단축키임 = %>%
# alt + - 는 <- 의 단축키!!! 
# 참고) # result = text %>% strsplit(",") %>% unlist %>% trimws %>% toupper %>% sort #는
# 참고) # result = sort(toupper(trimws(unlist(strsplit(text, ",")))))  이다.

filter(CO2,Plant=='Qn1') %>% ggplot(aes(1:7,uptake)) + geom_line()
# 아래와 같음.
#ggplot(filter(CO2,Plant=='Qn1') ,aes(1:7,uptake)) + geom_line()

#열들을 뽑고, 그 데이터를 가지고 summarize에 넣은 다음 완성된 결과를 past_CO2에 넣으라는 명령어. dplyr의 코드.
psat_CO2 <- group_by(CO2,Type,Plant,Treatment) %>% 
  summarize(uptakesum=sum(uptake)) #변수를 추가해서 요약한걸 보여줌.
psat_CO2$Plant <- substr(psat_CO2$Plant,3,3) #기존에서 이름바꾸고,

filter(psat_CO2,Type=='Quebec') %>% ggplot(aes(Plant,uptakesum,group=Treatment,color=Treatment))+
  geom_line(size=2) #출력 // group이라는 옵션을 사용함을 알기. 3차원 변수값에 대하여임.

#geom_line의 응용.
ggplot() + geom_line(data=filter(psat_CO2,Type=='Quebec'),aes(Plant,uptakesum,group=Treatment,color=Treatment),size=2) +
  geom_line(data=filter(psat_CO2,Type=='Mississippi'),aes(Plant,uptakesum,group=Treatment,color=Treatment),size=2,linetype='dashed')


#################geom_point##############
ggplot(iris,aes(Sepal.Width,Sepal.Length)) + geom_point()
ggplot(iris,aes(Sepal.Width,Sepal.Length,color=factor(Species),size=Petal.Length)) + geom_point()


#################geom_histogram##############
poissonNumber <- data.frame(Random_Number = rpois(500,4)) 
poissonNumber %>% ggplot(aes(Random_Number)) + geom_histogram()
#이게 된다는게 신기하다.... random_number변수에는 분명 저장되는게 없는데..
#변수에는 저장이 안된 상태로, 메모리 아끼려고 이렇게 일부러 쓰는거임.

poisson_random <- data.frame(lambda1=rpois(100,1),lambda5=rpois(100,5),lambda10=rpois(100,10))
#install.packages("tidyr")
library(tidyr)
poisson_random2 <- gather(poisson_random,lambda,value) #key ,value쌍으로 모음.
ggplot(poisson_random2,aes(value,fill=lambda)) + 
  geom_histogram(color='black',binwidth=1)

ggplot(poissonNumber,aes(Random_Number,..density..)) + 
  geom_histogram(binwidth=1,fill='white',color='black')

#################geom_density############## 
set.seed(501)
normalNumber <- data.frame(Numbers=round(rnorm(500,0,1),4))
ggplot(normalNumber,aes(Numbers))+geom_density()

data.frame(sigma1=round(rnorm(500,0,1),4),sigma2=round(rnorm(500,0,2),4) , 
           sigma3=round(rnorm(500,0,3),4)) %>% 
  gather(sigma,value) %>% ggplot(aes(value,fill=sigma)) + geom_density()
#gather은 tidyr에 있는 함수. dictionary라고 생각하면됨.


#################geom_boxplot############## 
ggplot(iris,aes(Species,Sepal.Length)) + geom_boxplot()
ggplot(iris,aes(Species,Sepal.Length,fill=Species)) + geom_boxplot(
  outlier.color = 'red',outlier.shape=23,outlier.size=9,
  outlier.alpha=0.5) 
#이상치의 바깥테두리 = 빨강, 모양=23번(다이아),크기=9,투명도 =0.5

ggplot(iris,aes(Species,Sepal.Length)) + geom_boxplot() +
  annotate('text',x='virginica',y=5,label='outlier')

number <- as.factor(rep(1:12,6)) #와우...
IS <- data.frame(InsectSprays,number)
ggplot(InsectSprays,aes(number,y=count,group=spray,color=spray)) +
  geom_line(size=2) #color을 넣으면 자동 그룹화가 되나봄.


#################annotate############## 
ggplot(Orange,aes(factor(age),circumference,group=Tree,color=Tree)) +
  geom_line(size=2) +
  annotate('rect',xmin=3,xmax=4,ymin=30,ymax=max(Orange$circumference),fill='aquamarine3',alpha=0.3)+
  #factor기에 xmin이 3임. 만약 factor가 아니라면, 실제 x값이 들어가게 되는것!! 주의!!
  annotate('text',x=3.5,y=190,label='diverging\nperiod',size=7)


data.frame(Year=1871:1970,Flow=as.numeric(Nile)) %>% ggplot(aes(Year,Flow)) + geom_line()
  
data.frame(Year=1871:1970,Flow=as.numeric(Nile)) %>% ggplot(aes(Year,Flow)) + geom_line()+
  annotate('rect',xmin=1871+which(Nile==min(Nile))-2.5 , xmax=1871 + which(Nile==min(Nile)) + 1 , ymin=min(Nile) , ymax=max(Nile), alpha=0.3, fill='cyan3') +
  annotate('segment',x=1871+which(Nile==min(Nile))+4,xend=1871+which(Nile==min(Nile))+16 , y=1200, yend=1200, arrow=arrow(ends='first',angle=20),size=1.5) +
  annotate('text',x=1871+which(Nile==min(Nile))+33,y=1205,label='drought period',size=8) +
  annotate('segment',x=1925,xend=1960,y=600,yend=600,size=1.5,color='skyblue3') +
  annotate('text',x=1943,y=550,label='stable period',size=8)


#################geom_abline##############
ggplot(iris,aes(Sepal.Length,Sepal.Width,color=Species))+
  geom_point()+geom_abline(intercept=-2.3,slope=1,size=2)

ggplot(iris,aes(Sepal.Length,Sepal.Width,color=Species))+
  geom_point()+geom_abline(intercept=-2.3,slope=1,size=2) + 
  xlim(0,8) + ylim(-3,4.5)

########### geom_hline&geom_vline #########
ggplot(iris,aes(Sepal.Width,Sepal.Length))+geom_point()+
  geom_hline(yintercept=6,size=2,color='cyan3')+
  geom_vline(xintercept=3.75,size=2,color='cyan3')


########### scale_x_B 자세한 용법은 ppt참고. 상당히 많다. #########
ggplot(iris,aes(Species,Sepal.Length)) + geom_boxplot() +
  scale_x_discrete(limits=c('virginica','setosa','versicolor'))
#limits는 순서를 바꾼것.(범주형일때)

ggplot(iris,aes(Species,Sepal.Length)) + geom_boxplot() +
  scale_x_discrete(limits=c('virginica','versicolor','setosa'),
                   labels=c('Group1','Group2','Group3'))
#Labels는 이름을 바꾼것.

ggplot(iris,aes(Sepal.Length,Sepal.Width,color=Species)) + geom_point()+
  scale_x_continuous(breaks=quantile(iris$Sepal.Length)) +
  scale_y_continuous(breaks=NULL)
#breaks는 눈금을 표시할 x값들 선정.

########### legend #########
ggplot(iris,aes(Species,Sepal.Width,fill=Species)) + geom_boxplot()+
  guides(fill=F)
# ggplot(iris,aes(Species,Sepal.Width,fill=Species)) + geom_boxplot()
# 위와 비교.

set.seed(501); CO2$index <- sample(1:84,replace=F)
ggplot(CO2,aes(x=index,y=uptake,color=Type,shape=Treatment)) +
  geom_point(size=4)+guides(shape=F)

########### Faceting #########
CO2$index <- rep(1:42,2)
ggplot(CO2,aes(index,uptake,color=Treatment)) + geom_point(size=3) +
  facet_grid(.~Type)

ggplot(CO2,aes(index,uptake,color=Treatment)) + geom_point(size=3) +
  facet_grid(Type~.)

CO2$Plant_type <- substr(CO2$Plant,3,3)
ggplot(CO2,aes(index,uptake,color=Treatment)) + geom_point(size=3) +
  facet_grid(Type~Plant_type)
#CO2라는 데이터안에 저장된 변수에서만 함수가 그려질 수 있음.

CO2$Plant_type[CO2$Plant_type==1] <- 'Plant1' 
#분할면의 이름을 바꾸고 싶으면, 데이터를 직접 바꿔야함. 어쩔수 없음.
ggplot(CO2,aes(index,uptake,color=Treatment)) + geom_point(size=3) +
  facet_grid(Type~Plant_type)

ggplot(CO2,aes(index,uptake,color=Treatment)) + geom_point(size=3) +
  facet_grid(Type~Plant_type,scale='free_y') #y scale을 자유롭게 변경


########### 슬라이드 54부터는 필요하면 보기. 상당히 minor한 영역 ###########