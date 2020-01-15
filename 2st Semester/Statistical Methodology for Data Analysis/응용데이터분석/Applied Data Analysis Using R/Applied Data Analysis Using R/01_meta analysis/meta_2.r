# Chapter 1. Meta Analysis
library(meta)

# Sec.3
library(meta)
data(Fleiss93cont)
Fleiss93cont
metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, sm="SMD", data=Fleiss93cont)

meta.2 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, sm="SMD", data=Fleiss93cont)
forest(meta.2, comb.fixed=FALSE, leftcols="studlab", rightcol=FALSE)
funnel(meta.2)

# end
