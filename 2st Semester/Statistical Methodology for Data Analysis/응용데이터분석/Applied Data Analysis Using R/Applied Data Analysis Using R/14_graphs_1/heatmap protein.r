# heat map of european protein data

library(RColorBrewer)
protein <- read.table("protein.txt",header=T)
par(oma=c(2,2,2,2))
heatmap(as.matrix(protein), Rowv=NA, Colv=NA, col = brewer.pal(10,"Reds"), scale="column", margins=c(5,8), xlab = "", ylab= "Countries", main = "Protein Uptake")

library(gclus)
(c.seq <- order.endlink(cor(protein)))
(r.seq <- order.endlink(-dist(protein)))

x11()
par(oma=c(2,2,2,2))
F <- scale(as.matrix(protein))
heatmap(F[r.seq,c.seq], Rowv=NA, Colv=NA, col = brewer.pal(10,"Reds"), scale="none", margins=c(5,8), xlab = "", ylab= "Countries", main = "Protein Uptake (Endlink)")

# end