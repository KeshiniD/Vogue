#

#CSTI 
mylogit <- glm(formula = CSTI ~ probiotics.2.months, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)