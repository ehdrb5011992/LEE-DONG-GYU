
rm(list=ls())
gc()

#install.packages("babynames")
#install.packages("mdsr")
#install.packages("Hmisc")
#install.packages("ggthemes")
#install.packages("extrafont")
#install.packages("grid")
library(babynames)
library(mdsr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(Hmisc)
library(extrafont)
library(grid)
windowsFonts(AR = windowsFont("Arial Unicode MS"))
baby_data <- make_babynames_dist()

Joseph <-  baby_data %>% filter(name == 'Joseph' , sex == 'M') %>%  # extract the data you need.
  mutate(count = count_thousands *  alive_prob  ) %>% 
  mutate(medi_count = 0) # for the blue bar.

medi <- with(Joseph,year) %>% wtd.quantile( weights = with(Joseph,est_alive_today) ,probs=0.5)
#medi <- with(Joseph,  wtd.quantile( year, weights = est_alive_today ,probs=0.5) ) 
medi_x_index <- which(with(Joseph,year)==medi)
medi_y_count <- with(Joseph,count)[medi_x_index]
Joseph[medi_x_index,"medi_count"] <- medi_y_count # input the median data


ans <- Joseph %>% ggplot(aes(x = year)) +  
  
  #data
  geom_bar(stat = "identity", aes(y = count) , fill = "#90D4E9", color = "white") +
  geom_bar(stat = "identity", aes(y = medi_count) , fill= "#5E68F2" , color = 'white',width=1) +
  geom_line(aes(y = count_thousands),size=1.8) + 
  geom_curve(x = 1992, xend = 1974, y = 43, yend = 23 , size=1.1 ,arrow = arrow(length = unit(0.3,"cm"),type = "closed"), curvature = 0.4) +
  
  #axis
  labs(x="",y="") +
  scale_x_continuous(breaks = seq(1900,2010,10) ,labels = c("1900",paste0("'",seq(10,90,10)),'2000',"'10"), limits=with(Joseph,c(min(year)-1,max(year)+1))) +
  scale_y_continuous(breaks = seq(0,40,10) ,labels = c(seq(0,30,10),'40k'), limits=c(0,41))+

  
  #title & caption
  ggtitle("Age Distribution of American Boys Named Joseph" , subtitle = 'By year of birth') + 
  labs(caption = "SOURCE: SOCIAL SECURITY ADMINISTRATION") +

  #text
  annotate('text',label = "Number of Josephs\nborn each year",x=1935,y=40,size=6,fontface =2,family='AR') +
  annotate('text',label = "Number of Josephs\nborn each year\nestimated to be alive\non Jan.1,2014",x=1915,y=12,size=6,color='#0095E5',fontface =2,family='AR') +
  annotate('text',label = "The median\nliving Joseph\nis 39 years old",x=2000,y=41,size=6,color='#545454',family='AR') + 

  
  #theme
  theme_fivethirtyeight() +

  theme(axis.text.x = element_text( size = 23)) +
  theme(axis.text.y = element_text( size = 23,hjust=-0.06)) +
   
  theme(plot.title = element_text(face = "bold", size = 30, color="#3f3f3f",hjust=0.9, family="AR")) +
  theme(plot.subtitle = element_text( size = 24, color="#3f3f3f" ,hjust=-0.06, family="AR")) +
  theme(plot.caption = element_text(size = 12 , color = "#888888",face="bold",family="AR")) +
 
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank() ) + 
  theme(panel.grid.major.x = element_line(color='#d3d3d3',size=1), panel.grid.major.y = element_line(color='#d3d3d3',size=1) ) +
  geom_hline(yintercept=0 , color = '#545454',size=1) 

gt = ggplot_gtable(ggplot_build(ans))
gt$layout$clip[gt$layout$name=="panel"] = "off" # clipping
grid.draw(gt)



