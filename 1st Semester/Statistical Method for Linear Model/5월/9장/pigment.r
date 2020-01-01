# This file is stored as   pigment.r

pigment <- read.table("pigment.dat", 
      col.names=c("Batch","Sample","Test","Y"))
pigment$Batch <- as.factor(pigment$Batch)
pigment$Sample <- as.factor(pigment$Sample)

# Random effects anaysis using lme()
library(nlme)
pigment.lme <- lme(Y ~ 1, random = ~1 | Batch/Sample, data=pigment)
summary(pigment.lme)

# Confidence intervals for fixed effects
# and estimated standard deviations (REML)
intervals(pigment.lme)

# ML estimation 
pigment.ml <- lme(Y ~ 1, random = ~1 | Batch/Sample, data=pigment, method="ML")
summary(pigment.ml)
intervals(pigment.ml)
