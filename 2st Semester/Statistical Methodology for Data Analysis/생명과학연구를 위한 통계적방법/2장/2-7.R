rm(list=ls())
data = read.table("C:\\Users\\82104\\Desktop\\data2.txt")
colnames(data) 
test = aov(value~health + diet,data)
summary(test)

library(agricolae)
comparison <- scheffe.test(test,"health", group=TRUE,console=TRUE)

library(agricolae)
comparison <- scheffe.test(test,"diet", group=TRUE,console=TRUE)       


