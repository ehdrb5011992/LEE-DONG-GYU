# Heat map of test matrix data

test.data <- read.table("test_matrix_1.txt",header=F)
F <- as.matrix(test.data)
n <- nrow(F)
p <- ncol(F)
rownames(F) <- 1:n
colnames(F) <- 1:p
par(oma = c(2,2,2,2))
heatmap(F, Rowv=NA, Colv=NA, col = c("yellow","black"), scale="none", xlab = "", ylab= "")

library(gclus)
(c.seq <- order.endlink(-dist(t(F))))
(r.seq <- order(F[,c.seq] %*% (1:p-(1+p)/2)))
x11()
par(oma = c(2,2,2,2))
heatmap(F[r.seq,c.seq], Rowv=NA, Colv=NA, col =c("yellow","black"), scale="none", xlab = "", ylab= "")

# end