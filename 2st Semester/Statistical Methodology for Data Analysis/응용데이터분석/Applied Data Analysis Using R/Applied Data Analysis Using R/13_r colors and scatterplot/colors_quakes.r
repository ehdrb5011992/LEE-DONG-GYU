# scatterplot using colors

data(quakes)
str(quakes)
plot(lat ~ long, col="red", data=quakes)
summary(quakes$depth)
attach(quakes)
depth.grp <- floor(depth/(max(depth)+1)*9)+1
plot(lat ~ long, col=brewer.pal(9,"Blues")[depth.grp], pch=20, main="quakes depth", cex=0.8)

# end