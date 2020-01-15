install.packages("tidyverse")
library(ggplot2)

ggplot(data=mpg)
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))

ggplot(data=mpg) + geom_point(mapping=aes(displ,y=hwy,color=class))
ggplot(mpg) + geom_point(aes(displ,hwy,size=class))
ggplot(mpg) + geom_point(aes(displ,hwy,alpha=class))
ggplot(mpg) + geom_point(aes(displ,hwy,shape=class))

ggplot(mpg) + geom_point(aes(displ,hwy),color='blue') #True
ggplot(mpg) + geom_point(aes(displ,hwy,color='blue')) #False

ggplot(mpg) + geom_point(aes(displ,hwy)) + facet_wrap(~class,nrow=2)
unique(mpg$class)
#얘를 그려보면 facet_wrap 과 facet_grid의 차이점을 알 수 있음.
ggplot(mpg)+geom_point(aes(displ,hwy))+facet_wrap(drv~cyl,nrow=2)
#그래서 이렇게는 잘 안씀

ggplot(mpg) + geom_point(aes(displ,hwy)) + facet_grid(drv~cyl)
unique(mpg$drv)

ggplot(mpg)+geom_point(aes(displ,hwy))+facet_grid(.~cyl)
# .~ 의경우로 쓴다면, facet_grid나 facet_wrap 이나 같음.
#grid는 2개의 비교에서 자주씀.

ggplot(mpg) + geom_point(aes(displ,hwy))
ggplot(mpg) + geom_smooth(aes(displ,hwy)) + geom_point(aes(displ,hwy))

#smooth는 group , linetype을 가지고 있음.
ggplot(mpg) + geom_smooth(aes(displ,hwy,linetype=drv)) +
  geom_point(aes(displ,hwy, color = drv))
ggplot(mpg) + geom_smooth(aes(displ,hwy,group=drv))
ggplot(mpg,aes(displ,hwy)) + geom_point() + geom_smooth()
ggplot(mpg , aes(displ,hwy)) + geom_point(aes(color=class)) + geom_smooth()
#install.packages("dplyr")
library(dplyr) #filter는 dplyr 함수.
ggplot(mpg, aes(displ,hwy)) + geom_point(aes(color=class)) + geom_smooth(
  data = filter(mpg,class=='subcompact'),se=FALSE)

ggplot(diamonds) + geom_bar(aes(x=cut))
str(diamonds) ;  unique(diamonds$cut)
# same as geom_bar
# ggplot(diamonds) + stat_count(aes(cut))

demo <- tribble(~a, ~b , 'bar_1',20,'bar_2',30,'bar_3',40) ; demo
ggplot(demo) + geom_bar(aes(a,b),stat='identity')

ggplot(diamonds) + geom_bar(aes(cut,..prop..,group=1))
#group=1을 해야 비율이 제대로 출력됨.
#group=2를 하든 , group='x'를 하든 상관없음. 반드시 있어야함.


ggplot(diamonds) + stat_summary(aes(cut,depth),
                                fun.ymin=min,
                                fun.ymax=max,
                                fun.y=median)
ggplot(diamonds) + geom_bar(aes(cut,color=cut))
ggplot(diamonds) + geom_bar(aes(cut,fill=cut))
ggplot(diamonds) + geom_bar(aes(cut,fill=clarity))
#colnames(diamonds)

#position='identity'는 각 박스가 자기 자신값을 이야기함.
ggplot(diamonds, aes(cut,fill=clarity)) + geom_bar(alpha=0.2 ,
                                                   position="identity")
#default는 쌓는거임. 
ggplot(diamonds, aes(cut,fill=clarity)) + geom_bar(alpha=0.2 ,
                                                   position="stack")
#position = 'fill' 은 비율을 출력해서 비교하기 쉽게 나옴.
ggplot(diamonds) + geom_bar(aes(cut,fill=clarity),position = 'fill')

#position = 'dodge' 는 재빨리움직이다? 흩어놓는거임.
#각각을 비교하기 쉽게 출력함.
ggplot(diamonds) + geom_bar(aes(cut,fill=clarity),position='dodge')

#position = jitter은 흩어놓는거임.
ggplot(mpg)+geom_point(aes(displ,hwy),position='jitter')

ggplot(mpg,aes(class,hwy))+ geom_boxplot()
ggplot(mpg,aes(class,hwy)) + geom_boxplot()+ coord_flip()
bar <- ggplot(diamonds) + geom_bar(aes(cut,fill=cut),
                                   show.legend=FALSE ,
                                   width=1) + theme(aspect.ratio=1) + labs(NULL,NULL)

bar + coord_flip()
bar + coord_polar()

ggplot(mpg,aes(displ,hwy,color=factor(cyl))) + geom_line() + 
  theme(legend.position='none')
#legend를 삭제 시킴.

ggplot(mpg,aes(displ,hwy,color=factor(cyl))) + geom_bar(stat='identity',position='identity',fill=NA)+
  theme(legend.position='none')
#position='identity'는 각 박스가 자기 자신값을 이야기함.
mpg$hwy
mpg$displ

ggplot(mpg,aes(displ,hwy,color=factor(cyl))) + geom_point() +
  geom_smooth(method='lm')
