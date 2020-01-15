# Chapter 10. Generalized linear models

# Poisson regression
diabetes <- read.csv("diabetes.csv", header=T)
str(diabetes)
diabetes.glm <- glm(deaths ~ age + gender, offset=l_popn, family="poisson", data=diabetes)
summary(diabetes.glm)
anova(diabetes.glm)

# logistic regression
car <- read.csv("car.csv", header=T)
str(car)
car.glm <- glm(clm ~ factor(agecat) + area + veh_body + factor(valcat), family="binomial", data=car)
summary(car.glm)
anova(car.glm)

# gamma regression
car_active <- read.csv("car_active.csv", header=T)
str(car_active)
car_active.glm <- glm(claimsize ~ factor(agecat) + gender + area + veh_body + factor(agecat)*gender , family=Gamma(link=log), data=car_active)
summary(car_active.glm)
anova(car_active.glm)

# end