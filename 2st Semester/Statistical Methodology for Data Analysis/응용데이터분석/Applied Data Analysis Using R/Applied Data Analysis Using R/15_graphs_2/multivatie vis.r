# visualizing multivariate dataset using diamond plot

library(plotrix)
data(mtcars)
str(mtcars)
dimnames(mtcars)[[1]] <- abbreviate(dimnames(mtcars)[[1]])
diamondplot(mtcars[,c(1,3,4,6)])

library(TSP)
D <- dist(scale(mtcars[,c(1,3,4,6)]))
D.tsp <- TSP(D, labels=rownames(mtcars))
tsp.mtcars <- solve_TSP(D.tsp, method="repetitive_nn")
labels(tsp.mtcars)
mtcars.1 <- mtcars[tsp.mtcars[1:32],]

# insert "diamontplot.r" here
diamondplot.1(mtcars.1[,c(1,3,4,6)],bg="grey80")

# end

