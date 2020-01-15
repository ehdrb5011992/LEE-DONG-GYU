# visualization of multivariate data by parallel coordinate plot

data(USJudgeRatings)
str(USJudgeRatings)

parcoord.2 <- function (x, col = 1, lty = 1, var.label = FALSE, ...) 
{
    rx <- apply(x, 2L, range, na.rm = TRUE)
    # x <- apply(x, 2L, function(x) (x-mean(x))/sd(x))
    # x <- apply(x, 2L, function(x)(x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    matplot(1L:ncol(x), t(x), type = "l", col = col, lty = lty, lwd=1.5,
        xlab = "", ylab = "", axes = FALSE, ylim=c(-4,4), xlim=c(-4,ncol(x)), ...)
    axis(1, at = 1L:ncol(x), labels = colnames(x), cex=0.5)
    legend(-4, 4, horiz=F, legend = rownames(x), lty = 1, col=rainbow(nrow(x)), cex = 0.8, lwd=1.5)
    for (i in 1L:ncol(x)) {
        lines(c(i, i), c(-4, 4), col = "grey70")
    }
    invisible()
}

X <- scale(USJudgeRatings)
rownames(X) <- 1:nrow(X)
windows(height=5, width=12)
parcoord.2(t(X),col=rainbow(ncol(X)))

library(gclus)
order <- order.single(-dist(X))
rownames(X) <- 1:nrow(X)
windows(height=5, width=12)
parcoord.2(t(X[order,]),col=rainbow(ncol(X)))

cor(USJudgeRatings)

# end


# end