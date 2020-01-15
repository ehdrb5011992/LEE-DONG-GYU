data = read.table("C:\\Users\\82104\\Desktop\\data1.txt")
colnames(data) ; unique(data$cell) ;

var.test(data[data$cell=="Tumor",1], data[data$cell=="Lymp",1]) #등분산 테스트.

t.test(data[data$cell=="Tumor",1], data[data$cell=="Lymp",1],  #satterthwait t-test
paired = FALSE, var.equal = FALSE, conf.level = 0.95,
alternative = "greater")

qqnorm(data[data$cell=="Tumor",1] , main = "Tumor") ;qqline(data[data$cell=="Tumor",1])
qqnorm(data[data$cell=="Lymp",1] ,  main = "Lymp") ; qqline(data[data$cell=="Lymp",1])
