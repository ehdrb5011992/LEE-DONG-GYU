# visualization of SVM for the spam data

library(kernlab)
data(spam)

x <- scale(spam[,1:57])
y <- spam[,58]
levels(y) <- c("nonspam","spam")

spam.svm <- ksvm(x, y, cost=10, kpar=list(sigma=0.1), prob.model=T)
spam.svm
table(predict(spam.svm), y)

n <- nrow(x); p <- ncol(x); half <- 1
importance <- rep(0,p)
for (j in 1:p){
    temp <- 0
    for (i in seq(2,n,5)){
    min <- x[i,j]-half
    max <- x[i,j]+half
    x.new <- matrix(rep(x[i,],9),byrow=T,nrow=9,ncol=p)
    x.new[,j] <- seq(min,max,length.out=9)  # (max-min)/8
    colnames(x.new) <- colnames(spam)[1:p]
    pred <- predict(spam.svm, newdata=x.new, type="probabilities") # "response"
    temp <- temp + max(pred[,2])-min(pred[,2])
    }
    importance[j] <- temp
}
output <- cbind(1:p, round(importance), rank(-importance))
rownames(output) <- colnames(x)
colnames(output) <- c("var","index","rank")
output

par(mfrow=c(2,2))
for (j in c(53,16,52,7)){
    for (i in 1:n){
    min <- x[i,j]-half
    max <- x[i,j]+half
    x.new <- matrix(rep(x[i,],9),byrow=T,nrow=9,ncol=p)
    x.new[,j] <- seq(min,max,length.out=9)  # (max-min)/8
    colnames(x.new) <- colnames(spam)[1:p]
    pred <- predict(spam.svm, newdata=x.new, type="probabilities") # "response"
    if(i>=2) par(new=T)
    plot(pred[,2]~seq(min,max,length.out=9),xlab=colnames(x.new)[j],ylab="spam",type="l",ylim=c(0,1),xlim=c(-4,4),col="#0000FF33")
  }
}

# end

> output  (systematic sampling 1/5, starting from 2, half=1)

                  var index rank
make                1    86   17
address             2    82   40
all                 3    87   13
num3d               4    81   56
our                 5    96    3
over                6    86   15
remove              7    86   18
internet            8    83   35
order               9    85   21
mail               10    85   20
receive            11    84   31
will               12    89    8
people             13    82   38
report             14    83   32
addresses          15    82   48
free               16    94    4
business           17    83   37
email              18    85   19
you                19    90    6
credit             20    82   47
your               21    90    7
font               22    94    5
num000             23    86   14
money              24    83   36
hp                 25    85   22
hpl                26    83   33
george             27    84   28
num650             28    82   49
lab                29    82   51
labs               30    83   34
telnet             31    81   55
num857             32    81   57
data               33    85   23
num415             34    81   54
num85              35    82   50
technology         36    82   41
num1999            37    84   30
parts              38    81   53
pm                 39    84   25
direct             40    82   44
cs                 41    82   46
meeting            42    82   39
original           43    82   43
project            44    84   29
re                 45    88    9
edu                46    87   12
table              47    82   52
conference         48    82   45
charSemicolon      49    84   24
charRoundbracket   50    87   11
charSquarebracket  51    82   42
charExclamation    52    97    2
charDollar         53    88   10
charHash           54    84   26
capitalAve         55    84   27
capitalLong        56    86   16
capitalTotal       57   100    1

> output (systematic sampling 1/5, starting from 1, half=0.5)
                  var index rank
make                1     9   22
address             2     8   44
all                 3    10   12
num3d               4     8   57
our                 5    16    2
over                6     8   28
remove              7     8   29
internet            8     8   46
order               9     8   33
mail               10     8   30
receive            11     8   38
will               12    11    5
people             13     8   39
report             14     8   50
addresses          15     8   53
free               16    10    7
business           17     8   34
email              18     9   23
you                19    10    8
credit             20     8   52
your               21     9   17
font               22    12    3
num000             23     8   31
money              24     8   42
hp                 25     9   19
hpl                26     8   26
george             27     9   13
num650             28     8   47
lab                29     8   43
labs               30     9   25
telnet             31     8   54
num857             32     8   55
data               33    10   10
num415             34     8   56
num85              35     8   45
technology         36     8   40
num1999            37     9   21
parts              38     8   49
pm                 39     8   35
direct             40     8   41
cs                 41     8   36
meeting            42     8   37
original           43     8   48
project            44     9   18
re                 45    10    9
edu                46    11    6
table              47     8   51
conference         48     8   32
charSemicolon      49     9   16
charRoundbracket   50    10   11
charSquarebracket  51     8   27
charExclamation    52    11    4
charDollar         53     9   24
charHash           54     9   20
capitalAve         55     9   15
capitalLong        56     9   14
capitalTotal       57    17    1
> 

> output (systematic sampling 1/5, starting from 2)
                  var index rank
make                1     8   25
address             2     8   40
all                 3     9   13
num3d               4     8   57
our                 5    14    2
over                6     8   22
remove              7     8   29
internet            8     8   44
order               9     8   31
mail               10     8   26
receive            11     8   32
will               12    10    6
people             13     8   41
report             14     8   45
addresses          15     8   54
free               16     9    8
business           17     8   36
email              18     8   23
you                19    11    5
credit             20     8   52
your               21     9   10
font               22    11    4
num000             23     8   30
money              24     8   42
hp                 25     9   17
hpl                26     8   27
george             27     9   12
num650             28     8   49
lab                29     8   46
labs               30     8   28
telnet             31     8   53
num857             32     8   56
data               33     9   15
num415             34     8   55
num85              35     8   48
technology         36     8   38
num1999            37     8   24
parts              38     8   51
pm                 39     8   35
direct             40     8   43
cs                 41     8   39
meeting            42     8   37
original           43     8   47
project            44     9   21
re                 45    10    7
edu                46     9    9
table              47     8   50
conference         48     8   34
charSemicolon      49     9   18
charRoundbracket   50     9   11
charSquarebracket  51     8   33
charExclamation    52    12    3
charDollar         53     9   16
charHash           54     9   19
capitalAve         55     9   20
capitalLong        56     9   14
capitalTotal       57    15    1


> output (old, whole sample)

                  var index rank
make                1    36   46
address             2    51   35
all                 3    45   40
num3d               4    48   37
our                 5   156    9
over                6    26   51
remove              7   227    4
internet            8    61   29
order               9    60   30
mail               10    55   32
receive            11    40   41
will               12    49   36
people             13    29   50
report             14    46   39
addresses          15    65   26
free               16   207    5
business           17    91   21
email              18    35   48
you                19    53   33
credit             20    68   24
your               21    91   20
font               22   117   16
num000             23   178    7
money              24   150   10
hp                 25   254    2
hpl                26   111   17
george             27   197    6
num650             28    18   55
lab                29    60   31
labs               30    18   54
telnet             31    88   22
num857             32    36   47
data               33    51   34
num415             34    47   38
num85              35    77   23
technology         36    66   25
num1999            37   144   11
parts              38    10   57
pm                 39    15   56
direct             40    21   52
cs                 41    61   28
meeting            42   124   13
original           43    37   45
project            44   130   12
re                 45    98   19
edu                46   160    8
table              47    30   49
conference         48    64   27
charSemicolon      49    19   53
charRoundbracket   50    38   44
charSquarebracket  51    39   42
charExclamation    52   302    1
charDollar         53   253    3
charHash           54    39   43
capitalAve         55   107   18
capitalLong        56   119   14
capitalTotal       57   118   15

# end of the output

