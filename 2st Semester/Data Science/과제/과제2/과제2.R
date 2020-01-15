rm(list=ls())
gc()
#install.packages("rmarkdown")
#install.packages("knitr")
library(ggplot2)
library(dplyr)
library(tidyr)
library(nycflights13)
library(ggthemes) # using in ggplot
library(grid) # can write outside of plot
library(lubridate) # time manipulation package
library(agricolae) # using in scheffe.test
str(flights)
real_flights = flights %>% filter( !(is.na(dep_time) & is.na(arr_time)))
?nnet
#### 1-1. What month had the highest proportion of cancelled flights?

sum(is.na(flights)) # The data set has "NA"
flights$month<-as.factor(flights$month)

dat1_1_1 <- flights  %>% mutate(cancelled_or_not = is.na(dep_time) & is.na(arr_time) )  %>%  # cancelled = TRUE 
 select(month,air_time,cancelled_or_not)  %>% group_by(month)

proportion <- summarize(dat1_1_1 , prop=mean(cancelled_or_not))
proportion
proportion[which.max(proportion$prop),] # highest proportion of cancelled flights


#### 1-2. What plane (specified by the tailnum variable) traveled the most times from NY city airports in 2013? 
####      Plot the number of trips per week over the year.

#the most times traveled plane
dat1_2_1 <- real_flights %>% select(tailnum) %>% group_by(tailnum) %>% summarize( count = n() ) %>% filter(!is.na(tailnum))
dat1_2_1[which.max(dat1_2_1$count),]

#plot 
dat1_2_2 <- temp <- real_flights %>% select(time_hour,tailnum) %>% mutate( week =  as.factor(yday(time_hour) %/% 7  + 1 ))  %>% 
  complete(tailnum,week)%>% filter(tailnum=="N725MQ")  %>% group_by(week) %>% summarize( count = n() ) 
dat1_2_2$count <- ifelse(temp$count==1 , 0,temp$count) 
dat1_2_2[which(dat1_2_2$count==max(dat1_2_2$count)),]

dat1_2_2 %>% ggplot() + 
  
  #data
  geom_bar(aes(x=week,y=count,fill=week),stat='identity',show.legend = FALSE) +
  
  #title & caption
  ggtitle(" The Number of Trips per Week over The Year" ) + 
  labs(caption = "Made by Lee Dong Gyu" ) +
  scale_x_discrete(breaks = seq(0,53,3) ,labels = seq(0,53,3))+
  
  #text & arrow
  annotate('segment',x=8,xend=10, y=18 , yend=18, arrow=arrow(ends='first',angle=20),size=1) +
  annotate('segment',x=29,xend=27, y=18 , yend=18, arrow=arrow(ends='first',angle=20),size=1) +
  annotate('text',label = "Maximum week : 8,19 / Count : 18",x=18.5,y=18,size=3.5,fontface =1) +
  
  #theme
  theme_fivethirtyeight() +
  
  theme(plot.title = element_text(face = "bold", size =30,hjust=0)) +
  theme(plot.caption = element_text(face="italic")) +
  theme(axis.text.y = element_text( face = "bold")) +
  
  geom_hline(yintercept=0 , color = '#545454',size=1) 


######################### Bonus #########################
#plot 
dat1_2_2_bonus <- real_flights %>% select(time_hour,tailnum) %>% mutate( week =  as.factor(yday(time_hour) %/% 7  + 1 )) %>% 
  group_by(week) %>% summarize( count = n() ) 

dat1_2_2_bonus[which.max(dat1_2_2_bonus$count),]

dat1_2_2_bonus %>% ggplot() + 
  
  #data
  geom_bar(aes(x=week,y=count,fill=week),stat='identity',show.legend = FALSE) +
  
  #title & caption
  ggtitle(" The Number of Trips per Week over The Year(Total)" ) + 
  labs(caption = "Made by Lee Dong Gyu" ) +
  scale_x_discrete(breaks = seq(0,53,3) ,labels = seq(0,53,3))+
  
  #text & arrow
  annotate('segment',x=29,xend=31, y=6636 , yend=6636, arrow=arrow(ends='first',angle=20),size=1) +
  annotate('text',label = "Maximum week : 29 , Count : 6636",x=40.5,y=6636,size=4,fontface =1) +

  #theme
  theme_fivethirtyeight() +

  theme(plot.title = element_text(face = "bold", size =15)) +
  theme(plot.caption = element_text(face="italic")) +
  theme(axis.text.y = element_text( face = "bold")) +
  
  geom_hline(yintercept=0 , color = '#545454',size=1) 

######################### Bonus End #########################

#### 2-1. What is the oldest plane (specified by tailnum variable) that flew from NY City airports in 2013?
####      How many airplanes that flew from NY City are included into the plane data?

# The oldest plane
dat2_1_1 <- planes %>% select(tailnum,year) %>% filter(!is.na(year))
dat2_1_1[with(dat2_1_1,which(year == min(year))),] 


# How many airplanes are included into the plane data
planes_tailnum <- planes %>% select(tailnum)
dat2_1_2 <- flights %>% semi_join(planes_tailnum)
# eqaul to this : dat2_1_2 <- flights %>% filter(tailnum %in% only_tailnum$tailnum)
length(unique(dat2_1_2$tailnum)) # All of the plane data's planes are included in the flights data


#### 2-2. How many planes have a missing date of manufactures?
####      What are the five most common manufacturers?
####      Has the distribution of manufacturer changed over time as reflected by the airplanes flying from NYC in 2013?

# How many planes have a missing date of manufactures?
dat2_2_1 <- planes %>% select(year) %>% filter(is.na(year))
nrow(dat2_2_1)

# What are the five most common manufacturers?
dat2_2_2 <- planes %>% select(manufacturer) %>% group_by(manufacturer) %>% summarize(count=n()) %>% arrange(desc(count))
dat2_2_2 %>%  head(5)

# Has the distribution of manufacturer changed?
count_top_7_planes<- dat2_2_2 %>%  head(7) 
count_the_other_sum_planes<- dat2_2_2[8:nrow(dat2_2_2),] %>% select(count) %>% sum() 
other1 <- tibble(manufacturer = "OTHER" , count=count_the_other_sum_planes)
dat2_2_3_planes <- count_top_7_planes %>%  rbind(other1) %>% mutate(prop = round(count / sum(count) , 2)) 


flights_planes_tailnum <- flights %>% inner_join(planes,by='tailnum')
flights_manufacturer_count <- flights_planes_tailnum %>% select(manufacturer) %>% group_by(manufacturer) %>% summarize(count=n()) %>% arrange(desc(count))
count_top_7_flights <- flights_manufacturer_count %>% head(7)
count_the_other_sum_flights <- flights_manufacturer_count[8:nrow(flights_manufacturer_count),] %>% select(count) %>% sum()
other2 <- tibble(manufacturer = "OTHER" , count=count_the_other_sum_flights)
dat2_2_3_flights <- count_top_7_flights %>%  rbind(other2) %>% mutate(prop = round(count / sum(count) , 2))


temp <- dat2_2_3_planes %>% left_join(dat2_2_3_flights , by = "manufacturer") %>%  
  select(manufacturer,prop.x , prop.y) %>% rename(Data_planes= prop.x ,Data_flights= prop.y )
final_dat2_2_3 <- temp %>% gather('Data_planes','Data_flights',key="Data_from",value="prop")

#plot
final_dat2_2_3 %>% ggplot(aes(x=manufacturer,y=prop,group=Data_from)) + 
  
  #data
  geom_bar(aes(fill=Data_from),stat='identity',position='dodge',width=0.7) +
  
  #text
  geom_text(aes(label=final_dat2_2_3$prop), position = position_dodge(width=0.7),vjust=-0.5) +
  
  #axis
  labs(x="",y="") +
  scale_x_discrete(label = final_dat2_2_3$manufacturer) +
  
  #title & caption
  ggtitle("Changes in the Manufacturer Distribution" ,subtitle = "Flights Data & Planes Data : Top 7 , Others") + 
  labs(caption = "Made by Lee Dong Gyu" ) +
  
  #theme
  theme(plot.title = element_text(face = "bold", size =30) ) +
  theme(plot.subtitle = element_text(face = "bold", size =15) ) +
  theme(plot.caption = element_text(face="italic")) +
  theme(axis.text.x = element_text( face = "bold",angle=45,hjust=1)) +
  theme(axis.text.y = element_text( face = "bold")) +
  
  geom_hline(yintercept=0 , color = '#545454',size=1) 


#### 3-1. What is the distribution of temperature in July,2013?
####      Identify any important outliers in terms of wind_speed variable.
####      What is the relationship between dewp and humid?

# Distribution of temp in July, 2013.
new <- weather %>% complete(year,month,day,hour) # Check the implicit missing each day : make new data set.
new %>% select(year,month,day) %>% is.na() %>% sum() # No missing days.

dat3_1_1 <- weather %>% filter(year==2013,month==7) %>% select(origin,year,month,day,hour,temp) %>% 
  group_by(origin,month,day) %>% summarize(avg_day_temp = mean(temp,na.rm=T) , max_day_temp = max(temp,na.rm=T) , min_day_temp = min(temp,na.rm=T) )

#plot
dat3_1_1 %>% ggplot(aes(x=day)) + 
  
  #data
  geom_line(aes(y=avg_day_temp,group=origin,color=origin),size=1.5) +
  geom_line(aes(y=max_day_temp,group=origin,color=origin),linetype='longdash',size=1) +
  geom_line(aes(y=min_day_temp,group=origin,color=origin),linetype='longdash',size=1) +

  #axis
  labs(y="",x="Day") +
  scale_x_continuous(breaks = seq(0,30,5) ,labels = seq(0,30,5))+
  
  #title & caption
  ggtitle("Distribution of Temperature in July, 2013" ,subtitle = "Min,Mean,Max Tempterature") + 
  labs(caption = "Made by Lee Dong Gyu" ) +
  
  #theme
  theme(legend.background = element_rect(fill="gainsboro",color="black")) +
  
  theme(plot.title = element_text(face = "bold", size =30) ) +
  theme(plot.subtitle = element_text(face = "bold", size =15) ) +
  theme(plot.caption = element_text(face="italic",size=10)) +
  theme(axis.text.x = element_text( face = "bold",size=13)) +
  theme(axis.text.y = element_text( face = "bold",size=13)) +
  theme(axis.title.x = element_text(face = "bold",size=15))


# Outliers in terms of wind_speed variable
# 원래는 돌풍변수(wind_gust)를 같이보는게 맞는것 같지만 여기선 wind_speed 관점
# 점들이 outlier이고, 30이상으로도도 많이 치솟아있는걸 확인 할 수 있다. 50이상은 전무함.
temp<- weather %>% select(origin,month,day,hour,wind_speed,wind_gust) 

temp  %>% mutate(day=as.factor(day)) %>%  ggplot() + 
  geom_boxplot(aes(x=day,y=wind_speed)) +
  facet_grid(.~month)

ind <- which(temp$wind_speed >100) # It might be a writing mistake.
temp$wind_speed[ind] 

dat3_1_2 <- temp %>% filter(wind_speed < 100)

#plot
dat3_1_2  %>% mutate(day=as.factor(day)) %>%  ggplot() +
  geom_boxplot(aes(x=day,y=wind_speed)) +
  facet_grid(.~month) +
  
  #axis
  labs(y="Wind Speed",x="Month - Day") +
  scale_x_discrete(breaks = seq(0,30,10) ,labels = seq(0,30,10))+
  
  #title & caption
  ggtitle("Outliers in terms of Wind speed Variable" ,subtitle = "Remove One Observation : Seems like a writing mistake") + 
  labs(caption = "Made by Lee Dong Gyu" ) +
  
  #theme
  theme(plot.title = element_text(face = "bold", size =30) ) +
  theme(plot.subtitle = element_text(face = "bold", size =15) ) +
  theme(plot.caption = element_text(face="italic",size=10)) +
  theme(axis.text.x = element_text( face = "bold",size=13)) +
  theme(axis.text.y = element_text( face = "bold",size=13)) +
  theme(axis.title.x = element_text(face = "bold",size=15)) +
  theme(axis.title.y = element_text(face = "bold",size=15))


# What is the relationship between dewp and humid?

dat3_1_3 <- weather %>% select(origin,dewp,humid) %>% filter(!is.na(humid),!is.na(dewp)) %>% rename(Dewpoint=dewp,Humidity=humid)

dat3_1_3 %>% ggplot(aes(x=Dewpoint,y=Humidity,shape=origin,col=origin)) + 
  geom_point(size=1.5) +
  geom_smooth() +

  #axis
  labs(y="Humidity",x="Dew Point") +
  scale_x_continuous(breaks = seq(0,100,10) ,labels = seq(0,100,10)) +
  scale_y_continuous(breaks = seq(0,100,10) ,labels = seq(0,100,10)) +
  
  #title & caption
  ggtitle("Relationship Between Dewpoint and Humidity" ) + 
  labs(caption = "Made by Lee Dong Gyu" ) +
  
  #theme
  theme(legend.background = element_rect(fill="gainsboro",color="black")) +
  theme(plot.title = element_text(face = "bold", size =30) ) +
  theme(plot.subtitle = element_text(face = "bold", size =15) ) +
  theme(plot.caption = element_text(face="italic",size=10)) +
  theme(axis.text.x = element_text( face = "bold",size=13)) +
  theme(axis.text.y = element_text( face = "bold",size=13)) +
  theme(axis.title.x = element_text(face = "bold",size=15)) +
  theme(axis.title.y = element_text(face = "bold",size=15))


######################### Bonus #########################
# Wind speed & Dewp // Wind speed & Humid
Bonus <- weather %>% select(origin,month,day,wind_speed,dewp,humid) %>% filter(wind_speed < 100 , !is.na(humid),!is.na(dewp)) %>% 
  group_by(month,day) %>% summarize(mean_wind_speed = mean(wind_speed),mean_dewp = mean(dewp),mean_humid = mean(humid)) 

#plot
#3D plot
with(Bonus,plot3D::scatter3D(x=mean_dewp, y=mean_humid, z=mean_wind_speed,
                                main="Mean: Wind Speed and (Dew Point,Humidity)",
                                xlab='Dew Point',theta = 15, d = 2,
                                ylab='Humidity',
                                zlab='Wind Speed',bty = "g",
                                pch = 20, cex = 2,
                                ticktype = "detailed") )
changed_Bonus <- Bonus %>% rename(Dewpoint=mean_dewp, Humidity=mean_humid) %>% 
  gather('Dewpoint','Humidity',key="Variables",value="Value")
# Mean Dewp & Mean Wind Speed
changed_Bonus %>% ggplot(aes(x=Value,y=mean_wind_speed)) + geom_point(aes(shape=Variables,col=Variables),size=3) +
  #axis
  labs(y="Mean: Wind Speed",x="Mean: Humidity & Dew Point") +
  scale_x_continuous(breaks = seq(0,100,10) ,labels = seq(0,100,10)) +
  
  #title & caption
  ggtitle("Mean: Wind Speed according to Dew Point and Humidity" ) + 
  labs(caption = "Made by Lee Dong Gyu" ) +
  
  #theme
  theme(legend.background = element_rect(fill="gainsboro",color="black")) +
  theme(plot.title = element_text(face = "bold", size =30) ) +
  theme(plot.subtitle = element_text(face = "bold", size =15) ) +
  theme(plot.caption = element_text(face="italic",size=10)) +
  theme(axis.text.x = element_text( face = "bold",size=13)) +
  theme(axis.text.y = element_text( face = "bold",size=13)) +
  theme(axis.title.x = element_text(face = "bold",size=15)) +
  theme(axis.title.y = element_text(face = "bold",size=15))
######################### Bonus End #########################


#### 3-2. On how many days was there precipitation in the NY are in 2013? 
####      Were there difrerences in the mean visibility (visib) based on the day of the week and/or month of the year?

# NY rain days
weather %>% select(month,day) %>% unique() %>% nrow() 
weather %>% select(month,day) %>% unique() %>%  group_by(month) %>% summarize(count=n()) 
View(weather %>% select(month,day) %>% unique() %>% filter(month==12) ) # Data set has a missing day on 12/31.

dat3_2_1 <- weather %>% select(year,month,day,hour,precip) %>% mutate(TF_precip = I(precip>0)) %>% select(month,day,TF_precip) %>% 
  group_by(month,day) %>% summarize(TF_rain=I(sum(TF_precip)!=0))

temp <- dat3_2_1 %>% select(month,TF_rain) %>% summarize(rain_days=sum(TF_rain)) 
sum(temp$rain_days)

# Difrerences mean visibility based on the day of the week and/or month
#bDifrerences mean visibility based on month and day
dat3_2_2_month_day <- weather %>% select(month,day,visib) %>% mutate(month=as.factor(month),day=as.factor(day)) 

# test Month & Day
test = aov(visib ~ month + day ,dat = dat3_2_2_month_day)
summary(test) 
# There exists a difference in monthly or daily average visibility groups

# Then, which day/month?
comparison <- agricolae::scheffe.test(test,'month',group=TRUE,console=TRUE)
comparison <- agricolae::scheffe.test(test,'day',group=TRUE,console=TRUE)



