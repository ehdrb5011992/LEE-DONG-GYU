# Chapter 14. Graphs 1
# treemap of Gross National Income data
library(treemap)
data(GNI2010)
str(GNI2010)
x11(); treemap(GNI2010,index=c("continent", "iso3"),vSize="population",vColor="GNI",type="value",bg.labels="yellow")

GNI2010$GNI.total <- GNI2010$population*GNI2010$GNI
GNI2010.a <- aggregate(GNI2010[,4:6],by=list(GNI2010$continent),sum)
GNI2010.a$GNI.percapita <- GNI2010.a$GNI.total/GNI2010.a$population
x11(); treemap(GNI2010.a,index=c("Group.1"),vSize="population",vColor="GNI.percapita",type="value",bg.labels="yellow")

# portfolio data
library(portfolio)
data(dow.jan.2005)
str(dow.jan.2005)
summary(dow.jan.2005$month.ret)
x11(); treemap(dow.jan.2005,index=c("sector","symbol"),vSize="price",vColor="month.ret",type="value",palette="RdYlGn",range=c(-0.13,0.05))

dow.jan.2005$profit <- dow.jan.2005$price*dow.jan.2005$month.ret
str(dow.jan.2005)
dow.jan.2005.a <- aggregate(dow.jan.2005[,c(3,7)],by=list(dow.jan.2005$sector),sum)
str(dow.jan.2005.a)
dow.jan.2005.a$month.ret <- dow.jan.2005.a$profit/dow.jan.2005.a$price
summary(dow.jan.2005.a$month.ret)
x11(); treemap(dow.jan.2005.a,index=c("Group.1"),vSize="price",vColor="month.ret",type="value",palette="RdYlGn",range=c(-0.06,0.04))

# end