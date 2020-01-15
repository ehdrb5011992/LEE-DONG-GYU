# sample size and power for proportions

library(pwr)
h <- ES.h(0.6,0.5)
h
pwr.p.test(h=h, n=50, sig.level=0.05, alternative="greater")
pwr.p.test(h=h, power=0.80, sig.level=0.05, alternative="greater")

# two proportions
h <- ES.h(0.6,0.4)
pwr.2p.test(h=h, n=50, sig.level=0.05, alternative="greater")
pwr.2p.test(h=h, power=0.8, sig.level=0.05, alternative="greater")

# end
