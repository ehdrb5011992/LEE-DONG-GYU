x <-matrix(c(15,3,21,2),byrow=T,2)
dimnames(x)=list(c("r","s"),c("o","x"))
x
#Fisher's exact test
fisher.test(x)
