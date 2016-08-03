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

#####################################################
#Aug-2-16
#what differences did we originally want to detect with following conditions:
pwr.t2n.test(n1=50, n2=300 , sig.level = 0.05, power=0.8)#like grant
# t test power calculation 
# n1 = 50
# n2 = 300
# d = 0.4291349
# sig.level = 0.05
# power = 0.8
# alternative = two.sided

#using this d, what power do we have with our sample size
pwr.t2n.test(n1=26, n2=300 , sig.level = 0.05, d=0.4291349)#like grant
# t test power calculation 
# n1 = 26
# n2 = 300
# d = 0.4291349
# sig.level = 0.05
# power = 0.5528921
# alternative = two.sided

#with sample size we have, what is the power to detect dif btw CSTs
#large differnces(0.1 small, 0.3 medium, 0.5 large)
pwr.chisq.test(w=0.5, df=5,sig.level=0.05, N=26)
#Chi squared power calculation 
#w = 0.5
#N = 26
#df = 5
#sig.level = 0.05
#power = 0.4671316
#NOTE: N is the number of observations
