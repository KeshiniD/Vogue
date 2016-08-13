#power
#with sample size we have, what is the power to detect dif btw CSTs
#large differnces(0.1 small, 0.3 medium, 0.5 large)
#df: groups -1
pwr.chisq.test(w=0.5, df=2,sig.level=0.05, N=54)
# Chi squared power calculation 
# w = 0.5
# N = 54
# df = 2
# sig.level = 0.05
# power = 0.9185674
# NOTE: N is the number of observations

pwr.chisq.test(w=0.3, df=2,sig.level=0.05, N=54)
# Chi squared power calculation 
# w = 0.3
# N = 54
# df = 2
# sig.level = 0.05
# power = 0.491749
 
# NOTE: N is the number of observations

pwr.chisq.test(w=0.1, df=2,sig.level=0.05, N=54)
# Chi squared power calculation 
# w = 0.1
# N = 54
# df = 2
# sig.level = 0.05
# power = 0.0929194
 
# NOTE: N is the number of observations

#comparasion