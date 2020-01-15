# Chapter 14. Graphs 1
# mosaic plot of Berkeley Admissions data
data(UCBAdmissions)
str(UCBAdmissions)
addmargins(apply(UCBAdmissions, c(2,1), sum))
require(graphics)
mosaicplot(apply(UCBAdmissions, c(2,1), sum), color=c("red","gray"), main="UC Berkeley Admissions")
x11(); mosaicplot(~Dept+Gender+Admit, data=UCBAdmissions, color=c("red","gray"), dir=c("v","v","h"), off=1, main = "UC Berkeley Admissions")

# mosiac plot of Titanic data
require(stats)
data(Titanic)
str(Titanic)
mosaicplot(~ Sex + Survived, data = Titanic, color=c("gray","blue"), dir=c("v","h"), off=1)
x11(); mosaicplot(~ Age + Sex + Survived, data = Titanic, color=c("gray","blue"), dir=c("v","v","h"), off=1)
x11(); mosaicplot(~ Class + Sex + Survived, data = Titanic, color=c("gray","blue"), dir=c("v","v","h"), off=1)

# end


