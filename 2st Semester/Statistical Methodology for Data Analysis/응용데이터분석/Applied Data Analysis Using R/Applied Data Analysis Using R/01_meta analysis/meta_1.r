# Chapter 1. Meta Analysis
library(meta)

# Sec.1 and Sec.2
data(Fleiss93)
Fleiss93
metabin(event.e, n.e, event.c, n.c, studlab=paste(study, year), sm="OR", method="inverse", data=Fleiss93)
meta.1 <- metabin(event.e, n.e, event.c, n.c, studlab=paste(study, year), sm="OR", method="inverse", data=Fleiss93)
forest(meta.1, comb.fixed=FALSE, leftcols="studlab", rightcol=FALSE)
funnel(meta.1)

# Sec.3
data(Fleiss93cont)
Fleiss93cont
metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, sm="SMD", data=Fleiss93cont)
meta.2 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, sm="SMD", data=Fleiss93cont)
forest(meta.2, comb.fixed=FALSE, leftcols="studlab", rightcol=FALSE)
funnel(meta.2)

# end
