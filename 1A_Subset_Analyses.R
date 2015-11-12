library(pwr)
pwr.t2n.test(n1 = 26, n2=104 , sig.level = 0.05, power = 0.8)
# t test power calculation 
#n1 = 26
#n2 = 104
#d = 0.6189457 (can detect medium differences)
#sig.level = 0.05
#power = 0.8
#alternative = two.sided

pwr.2p2n.test(n1 = 26, n2=104 , sig.level = 0.05, power = 0.8)
#difference of proportion power calculation for binomial distribution (arcsine transformation) 
#h = 0.6142887 (can detect medium differences)
#n1 = 26
#n2 = 104
#sig.level = 0.05
#power = 0.8
#alternative = two.sided
#NOTE: different sample sizes
