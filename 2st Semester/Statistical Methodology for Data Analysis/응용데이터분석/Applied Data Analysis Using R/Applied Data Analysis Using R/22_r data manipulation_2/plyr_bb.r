# plyr for baseball data
library(plyr)
str(baseball)

calculate_c.year <- function(df) mutate(df, c.year = year - min(year)+1)
baseball.1 <- ddply(baseball, .(id), calculate_c.year)
head(baseball.1, 10)

calculate_c.rbi <- function(df) c(best.year=df$c.year[which.max(df$rbi)], best.rbi=max(df$rbi), career.year=max(df$c.year))
bb.2 <- ddply(baseball.1, .(id), calculate_c.rbi)
str(bb.2)

# histograms of best.year and career.year
max(bb.2$career.year)
bb.2[which.max(bb.2$career.year),]

windows(height=5, width=8)
hist(bb.2$best.year, breaks=seq(0.5,40.5,1), xlab="best.year", main="")
windows(height=5, width=8)
hist(bb.2$career.year, breaks=seq(0.5,40.5,1), xlab="career.year", main="")

bb.3 <- ddply(baseball, "id", summarise, career.year = max(year)-min(year)+1, nteams = length(unique(team)))
str(bb.3)

# end
