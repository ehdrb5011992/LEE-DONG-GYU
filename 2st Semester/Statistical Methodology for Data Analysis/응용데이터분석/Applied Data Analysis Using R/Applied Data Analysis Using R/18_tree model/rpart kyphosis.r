# Chapter 18. Tree model via rpart package
# kyophosis data

library(rpart)
data(kyphosis)
str(kyphosis)
table(kyphosis$Kyphosis)

tree.1 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
tree.1
par(mar=c(1,1,1,1), xpd = TRUE); plot(tree.1)
text(tree.1, use.n = TRUE)

tree.2 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, parms = list(split = "information"))
tree.2
par(mar=c(1,1,1,1), xpd = TRUE); plot(tree.2)
text(tree.2, use.n = TRUE)

table(kyphosis$Kyphosis, predict(tree.1, type="class"))
table(kyphosis$Kyphosis, predict(tree.2, type="class"))

kyphosis$wts <- ifelse(kyphosis$Kyphosis=="present", 0.79, 0.21)

tree.2a <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, parms = list(split = "information"), weights=wts)
par(mar=c(1,1,1,1), xpd = TRUE); plot(tree.2a)
text(tree.2a, use.n = TRUE)

table(kyphosis$Kyphosis, predict(tree.2a, type="class"))

# end