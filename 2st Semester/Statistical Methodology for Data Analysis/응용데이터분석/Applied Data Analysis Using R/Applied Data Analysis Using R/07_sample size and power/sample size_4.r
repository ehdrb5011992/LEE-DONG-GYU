# for correlations
library(pwr)
pwr.r.test(r=0.3, n=50, sig.level=0.05, alternative="greater")
pwr.r.test(r=0.3, power=0.8, sig.level=0.05, alternative="greater")

# one-sided "less"
pwr.t.test(d=-0.2, n=80, sig.level=0.05, type="one.sample",alternative="less")
pwr.t.test(d=0.2, n=80, sig.level=0.05, type="one.sample",alternative="greater")

# end