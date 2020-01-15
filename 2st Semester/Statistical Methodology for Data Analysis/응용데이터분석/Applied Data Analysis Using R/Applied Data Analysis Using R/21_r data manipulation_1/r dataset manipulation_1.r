# Appendix I: Data manipulaltion

library(Matching)
data(lalonde)
str(lalonde)
table(lalonde$treat)

# subsetting 1
lalonde.1 <- subset(lalonde,treat==1)
lalonde.2 <- subset(lalonde,treat==0)
str(lalonde.1)
str(lalonde.2)

# subsetting 2
grp <- factor(lalonde$treat)
levels(grp) <- c("control","treatment")
lalonde.s <- split(lalonde,grp)
names(lalonde.s)
str(lalonde.s$treatment)
str(lalonde.s$control)

# subsetting 3
grp.1 <- lalonde[lalonde$treat==1,]
grp.0 <- lalonde[lalonde$treat==0,]
str(grp.1)
str(grp.0)

set.seed(123); id <- sample(1:nrow(lalonde))
lalonde.a <- cbind(id,lalonde)
str(lalonde.a)

lalonde.d <- lalonde.a[,c("id","age","educ","black","hisp","married","nodegr","treat")]
lalonde.f <- lalonde.a[,c("id","re74","re75","re78","u74","u75")]
str(lalonde.d)
str(lalonde.f)
head(lalonde.d)
head(lalonde.f)

# subset selection
set.seed(123); id <- sample(1:nrow(lalonde))
lalonde.id <- cbind(id,lalonde)
str(lalonde.id)
lalonde.id.demo <- subset(lalonde.id,select=c(id,age,educ,black,hisp,married,nodegr,treat))
lalonde.id.field <- subset(lalonde.id,select=c(id,re74,re75,re78,u74,u75))
str(lalonde.id.demo)
str(lalonde.id.field)

# ordering by id
lalonde.id.demo.1 <- lalonde.id.demo[order(lalonde.id.demo$id),]
str(lalonde.id.demo.1)
head(lalonde.id.demo.1)
head(lalonde.id.field)

# using reshape package
library(reshape)
lalonde.id.demo.2 <- sort_df(lalonde.id.demo,vars="id")
head(lalonde.id.demo.2)
identical(lalonde.id.demo.1,lalonde.id.demo.2)

lalonde.id.demo.3 <- sort_df(lalonde.id.demo,vars=c("educ","age"))
head(lalonde.id.demo.3,10)


# combining two datasets in vertical direction
lalonde.12 <- rbind(lalonde.1, lalonde.2) 
str(lalonde.12)
identical(lalonde,lalonde.12)
identical(rbind(lalonde.1, lalonde.2),rbind(lalonde.2,lalonde.1))

# combining two datasets in horizontal direction
lalonde.id.demo.field <- cbind(lalonde.id.demo,lalonde.id.field[,-1])
str(lalonde.id.demo.field)
identical(lalonde.id,lalonde.id.demo.field) # FALSE

# merging two datasets by id
lalonde.merged <- merge(lalonde.id.demo.1,lalonde.id.field,by="id")
str(lalonde.merged)
identical(lalonde.id,lalonde.merged)

lalonde.merged.1 <- merge(lalonde.id.field,lalonde.id.demo.1,by="id")
str(lalonde.merged.1)
identical(lalonde.merged,lalonde.merged.1)

# aggregated statistics
lalonde.treat <- aggregate(lalonde,by=list(lalonde$treat),mean)
str(lalonde.treat)
lalonde.treat[,c("treat","re74","re75","re78","u74","u75")]

lalonde.treat.race <- aggregate(lalonde,by=list(lalonde$treat,lalonde$black,lalonde$hisp),mean)
lalonde.treat.race[,c("treat","black","hisp","re74","re75","re78","u74","u75")]

# data summary
lalonde.summary <- apply(lalonde[,7:11],2,summary)
lalonde.summary
class(lalonde.summary)

# screening
lalonde.age50 <- lalonde[lalonde$age>=50,]
str(lalonde.age50)
lalonde.age50[,1:6]

lalondde.black.treat <- lalonde[lalonde$black==1 & lalonde$treat==1,]
str(lalondde.black.treat)

# end