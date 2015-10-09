#variance of log of odds ratio
lores(lor, var.lor, n.1, n.2,
      level = 95, cer = 0.2, dig = 2, verbose = TRUE, id=NULL, data=NULL)
lores(2,.3,4,5)

var(exp(x))
x <- log(9.87)
class(x)
x

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

#rememver to factor variables
test <- multinom(CST ~ Age.cat, data = total) #multinomial for CST, works
summary(test)
#calculate the p-values using Wald's test (z-tests)
z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
