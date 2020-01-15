rm(list=ls())

#install.packages("epitools")
library(epitools)
data <- matrix(c(5,21,8,82),byrow=TRUE,nrow=2)
rr <- epitab(data,method="riskratio",rev="b") 
rr$tab
rr2 <- epitab(data,method = "oddsratio" , rev="b")
rr2
