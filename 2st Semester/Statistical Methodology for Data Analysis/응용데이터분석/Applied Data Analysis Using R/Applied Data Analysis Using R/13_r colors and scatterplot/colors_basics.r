# Chapter 13. R Colors and Scatterplot

# Footnote 2:
  color <- c("#000000","#FF0000","#00FF00","#0000FF","#FFFFFF")
  pie(rep(1,5),col=color,labels=color)
  par(new=T)
  pie(rep(1,1),col="white",radius=0.5,labels="")

# Footnote 3:
  windows(height=2.5, width=8); n <- 10
  barplot(rep(1,n),col=rainbow(n,alpha=1),axes=F,main="rainbow colors")
  barplot(rep(1,n),col=heat.colors(n,alpha=1),axes=F,main="heat colors")
  barplot(rep(1,n),col=terrain.colors(n,alpha=1),axes=F,main="terrain colors")
  barplot(rep(1,n),col=topo.colors(n,alpha=1),axes=F,main="topo colors")
  barplot(rep(1,n),col=cm.colors(n,alpha=1),axes=F,main="cyan-magenta colors")

# Footnote 4:
  library(RColorBrewer)
  display.brewer.all(type="seq")

# Footnote 5:
  windows(height=2.5, width=8)
  barplot(rep(1,9),col=brewer.pal(9,"Reds"),axes=F,main="Brewer Reds")
  barplot(rep(1,9),col=brewer.pal(9,"Greens"),axes=F,main="Brewer Greens")

# Footnote 6:
  library(RColorBrewer)
  display.brewer.all(type="div")
  x11(); display.brewer.all(type="qual")

# end
