# TSP for protein data

library(TSP)
protein <- read.table("protein.txt",header=T)
str(protein)
country <- protein$Country
X <- scale(protein[,2:10])
D <- dist(X)
D.tsp <- TSP(D,labels=country)
solve_TSP(D.tsp,method="repetitive_nn")

solve_TSP(D.tsp,method="2-opt")

tsp.country <- solve_TSP(D.tsp,method="2-opt")
labels(tsp.country)

D.plus <- insert_dummy(D.tsp, label = "cut")
tsp.country.plus <- solve_TSP(D.plus, method="2-opt")
tsp.country.plus
tsp.country.cut <- cut_tour(tsp.country.plus, "cut")
labels(tsp.country.cut)

mds <- cmdscale(D)
plot(mds[,1],mds[,2],type="n", xlim=c(-5,5),ylim=c(-5,5),main="Protein Consumption")
text(mds[,1],mds[,2],abbreviate(country,minlength=3),cex=0.8)
seq.mds <- as.integer(tsp.country.cut)
for (i in 2:length(country)){
    segments(mds[seq.mds[i-1],1],mds[seq.mds[i-1],2],mds[seq.mds[i],1],mds[seq.mds[i],2],col="red")
}
# end