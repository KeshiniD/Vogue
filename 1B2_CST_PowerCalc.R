#load package
library(pwr)

#Cohen's d: small(0.1), medium (0.3), large (0.5) effect sizes
#detect small, medium, large differences between CSTs
#df = groups - 1

#small differences
pwr.chisq.test(w=0.1, df=5,sig.level=0.05, power=0.8)
#Chi squared power calculation 
#w = 0.1
#N = 1282.761
#df = 5
#sig.level = 0.05
#power = 0.8
#NOTE: N is the number of observations

#medium differences
pwr.chisq.test(w=0.3, df=5,sig.level=0.05, power=0.8)
#Chi squared power calculation 
#w = 0.3
#N = 142.529
#df = 5
#sig.level = 0.05
#power = 0.8
#NOTE: N is the number of observations

#large differnces
pwr.chisq.test(w=0.5, df=5,sig.level=0.05, power=0.8)
#Chi squared power calculation 
#w = 0.5
#N = 51.31043
#df = 5
#sig.level = 0.05
#power = 0.8
#NOTE: N is the number of observations