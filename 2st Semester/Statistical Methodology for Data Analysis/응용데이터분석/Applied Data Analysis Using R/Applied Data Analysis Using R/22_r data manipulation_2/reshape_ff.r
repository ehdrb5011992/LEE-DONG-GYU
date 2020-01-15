# Data manipulation using reshape package

library(reshape)
data(french_fries)
str(french_fries)
colnames(french_fries)[5:9] <- c("y1", "y2", "y3", "y4","y5")
head(french_fries, 10)
# N <- nrow(french_fries)
# french_fries <- french_fries[sample(1:N),]

ff.melted <- melt(french_fries, id=c("time","subject","treatment","rep"), na.rm=TRUE)
str(ff.melted)
head(ff.melted, 10)

cast(ff.melted, subject+treatment ~ variable, length)
cast(ff.melted, subject+treatment ~ variable, function(x) 20-length(x))

options(digits=3)
cast(ff.melted, subject+treatment ~ variable, mean)
cast(ff.melted, subject+treatment ~ variable, mean, margins="grand_col")

options(digits=3)
cast(ff.melted, subject ~ treatment ~ variable, mean)
apply(cast(ff.melted, subject ~ treatment ~ variable, mean), c(2,3), mean)

options(digits=3)
cast(ff.melted, subject+treatment ~ ., quantile, c(0,0.25,0.5,0.75,1))

# end