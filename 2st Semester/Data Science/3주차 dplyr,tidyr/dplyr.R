#install.packages("tidyverse")
#install.packages("nycflights13")
library(tidyverse)
library(nycflights13)

flights
#view(flights) 다보려면 view함수에 넣어줘야함.

jan1 <- filter(flights,month==1,day==1)
jan1
filter(flights,month==11|month==12)
filter(flights,month %in% c(11,12)) #위와 같음. 개쩐당.
#dplyr의 연산자가아닌, global한 r의 연산자.

filter(flights,!(arr_delay>120 | dep_delay>12))
filter(flights,arr_delay <= 120 , dep_delay <= 12)

#is.na() and near() are useful.
(0.2+0.7) == 0.9 #0.7이 2진수를 바꾸는 과정에서 두 값은 다르게 된다.
near(0.2+0.7,0.9) #이렇게 써야한다. 
#rounding error때문에 그럼. near함수를 습관화하기!!!
#사실 == 기호는 매우 위험한 코딩임.

select(flights,year,month,day)
select(flights,year:day)
select(flights,-(year:day))

select(flights,starts_with("yea"))
select(flights,time_hour,air_time,everything())       

flights_sml <- select(flights,year:day,ends_with('delay'),distance,air_time)
mutate(flights_sml,gain=arr_delay-dep_delay,speed=distance/air_time)

arrange(flights_sml, distance)
arrange(flights_sml, distance, year, month, day)

summarize(flights, N = n(), delay = mean(dep_delay, na.rm = TRUE))
#n()은 데이터 길이를 의미하는 함수.
#summarize함수는 group_by와 함께 쓰일 시 굉장한 시너지를 냄.
by_day <- group_by(flights,year,month,day)
summarize(by_day , N=n(),delay=mean(dep_delay,na.rm=F))

by_dest <- group_by(flights, dest)
delay <- summarize(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dest != "HNL")

#파이프 표현. 위처럼 쓰는것 대신 아래처럼 써서 효과적으로 표현 가능.
delays<- flights %>% group_by(dest) %>% summarize(count=n(),dist=mean(distance,na.rm=T),
                                                  delay=mean(arr_delay,na.rm=T)) %>% 
  filter(count>20,dest!='HNL')


