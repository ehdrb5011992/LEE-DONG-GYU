# visualization of rf for the spam data

library(randomForest)
library(kernlab)
data(spam)
half <- 1
x <- scale(spam[,1:57])
y <- spam[,58]
levels(y) <- c("nonspam","spam")

spam.rf <- randomForest(x, y)
spam.rf
table(predict(spam.rf), y)
n <- nrow(x)
p <- ncol(x)

library(randomForest)
spam.1 <- spam[spam$type=="spam",]
spam.0 <- spam[spam$type=="nonspam",]
spam.balanced <- rbind(spam.1,spam.1,spam.1,spam.0,spam.0)
spam.rf <- randomForest(type ~ ., data=spam.balanced)

table(predict(spam.rf), spam.balanced$type)

importance <- rep(0,p)
for (j in 1:p){
    temp <- 0
    for (i in seq(2,n,5)){
    min <- x[i,j]-half
    max <- x[i,j]+half
    x.new <- matrix(rep(x[i,],9),byrow=T,nrow=9,ncol=p)
    x.new[,j] <- seq(min,max,length.out=9)  # (max-min)/8
    colnames(x.new) <- colnames(spam)[1:p]
    pred <- predict(spam.rf, newdata=x.new, type="prob") # "response"
    temp <- temp + max(pred[,2])-min(pred[,2])
    }
    importance[j] <- temp
}

output <- cbind(1:p, round(importance), rank(-importance))
rownames(output) <- colnames(x)
colnames(output) <- c("var","index","rank")
output

par(mfrow=c(2,2))
for (j in c(7,16,52,53)){
    for (i in seq(1,n,4)){
    min <- x[i,j]-half
    max <- x[i,j]+half
    x.new <- matrix(rep(x[i,],9),byrow=T,nrow=9,ncol=p)
    x.new[,j] <- seq(min,max,length.out=9)  # (max-min)/8
    colnames(x.new) <- colnames(spam)[1:p]
    pred <- predict(spam.rf, newdata=x.new, type="prob") # "response"
    if(i>=2) par(new=T)
    plot(pred[,2]~seq(min,max,length.out=9),xlab=colnames(x.new)[j],ylab="spam",
         type="l",ylim=c(0,1),xlim=c(-4,4),col="#0000FF33")
  }
}

# end

> output (1/5 sample, startng from 2, half=1)
                  var index rank
make                1     5   48
address             2     9   38
all                 3    10   35
num3d               4    18   22
our                 5    20   21
over                6     9   37
remove              7   266    1
internet            8    44   11
order               9     8   41
mail               10    14   25
receive            11    61    8
will               12     8   39
people             13     5   51
report             14     7   46
addresses          15     7   45
free               16   108    2
business           17    41   14
email              18    14   26
you                19    12   29
credit             20    45    9
your               21    28   16
font               22    45   10
num000             23    63    7
money              24    64    6
hp                 25    42   13
hpl                26    26   18
george             27    77    5
num650             28    26   17
lab                29    13   28
labs               30    17   23
telnet             31     6   47
num857             32     5   49
data               33     5   52
num415             34     5   50
num85              35     7   43
technology         36    11   30
num1999            37    15   24
parts              38     2   54
pm                 39    10   33
direct             40    13   27
cs                 41    10   36
meeting            42    31   15
original           43     8   40
project            44    11   32
re                 45    20   20
edu                46    44   12
table              47     2   53
conference         48     7   42
charSemicolon      49    11   31
charRoundbracket   50    23   19
charSquarebracket  51     7   44
charExclamation    52   101    3
charDollar         53    80    4
charHash           54    10   34
capitalAve         55     1   55
capitalLong        56     0   57
capitalTotal       57     0   56

> end

