# sample size and power
library(pwr)

## one sample (power)
pwr.t.test(d=0.2, n=80, sig.level=0.05, type="one.sample", alternative="greater")
pwr.t.test(d=0.2, power=0.8, sig.level=0.05, type="one.sample", alternative="greater")

## paired samples (power)
d <- 0.25/sqrt(2*(1-0.5))
pwr.t.test(d=d, n=50, sig.level=0.05, type="paired", alternative="greater")
pwr.t.test(d=d, n=50, sig.level=0.05, type="one.sample", alternative="greater")
pwr.t.test(d=d, power=0.8, sig.level=0.05, type="paired", alternative="greater")

## two independent samples (power)
d <- 0.4
pwr.t.test(d=d, n=50, sig.level=0.05, type="two.sample", alternative="greater")
pwr.t.test(d=d, power=0.8, sig.level=0.05, type="two.sample", alternative="greater")

# end
 