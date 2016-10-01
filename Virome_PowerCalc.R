#load package
library(pwr)

#Cohen's d: small(0.1), medium (0.3), large (0.5) effect sizes
#detect small, medium, large differences between CSTs
#df = groups - 1

#small differences
pwr.chisq.test(w=0.1, df=2,sig.level=0.05, power=0.8)
# Chi squared power calculation 
# w = 0.1
# N = 963.46889
# df = 2
# sig.level = 0.05
# power = 0.8
# NOTE: N is the number of observations

#medium differences
pwr.chisq.test(w=0.3, df=2,sig.level=0.05, power=0.8)
# Chi squared power calculation 
# w = 0.3
# N = 107.0521
# df = 2
# sig.level = 0.05
# power = 0.8
# NOTE: N is the number of observations

#large differnces
pwr.chisq.test(w=0.5, df=2,sig.level=0.05, power=0.8)
# Chi squared power calculation 
# w = 0.5
# N = 38.538748
# df = 2
# sig.level = 0.05
# power = 0.8
# NOTE: N is the number of observations

#################################
#what power have with current sample sizes
#small differences
pwr.chisq.test(w=0.1, df=2,sig.level=0.05, N=54)
# Chi squared power calculation 
# w = 0.3
# N = 54
# df = 2
# sig.level = 0.05
# power = 0.49174896
# NOTE: N is the number of observations

#medium differences
pwr.chisq.test(w=0.3, df=2,sig.level=0.05, N=54)
# Chi squared power calculation 
# w = 0.3
# N = 54
# df = 2
# sig.level = 0.05
# power = 0.49174896
# NOTE: N is the number of observations

#large differences
pwr.chisq.test(w=0.5, df=2,sig.level=0.05, N=54)
# Chi squared power calculation 
# w = 0.5
# N = 54
# df = 2
# sig.level = 0.05
# power = 0.91856737
# NOTE: N is the number of observations

###############
#with sample sizes, what can detect with power 80%
pwr.chisq.test(df=2,sig.level=0.05, N=54, power=0.8)
# Chi squared power calculation 
# w = 0.42239673
# N = 54
# df = 2
# sig.level = 0.05
# power = 0.8
# NOTE: N is the number of observations