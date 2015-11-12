#load packages
library(pwr)

#Power Calculations
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

#Diversity Indices
#load packages
library(vegan)
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)
library(entropart)
library(epitools)

#load dataset
data <- read.csv(file.path("data1A_1B2.csv"))

#select subset of participants wish to analyze
data2 <- data %>%
  select(Bacterial.Species, Vogue1A.01.003, Vogue1A.01.007, Vogue1A.01.012, 
         Vogue1A.01.016, Vogue1A.01.017, Vogue1A.01.019, Vogue1A.01.020, 
         Vogue1A.01.022, Vogue1A.01.023, Vogue1A.01.024, Vogue1A.01.025, 
         Vogue1A.01.026, Vogue1A.01.029, Vogue1A.01.030, Vogue1A.01.033, 
         Vogue1A.01.034, Vogue1A.01.036, Vogue1A.01.041, Vogue1A.01.042, 
         Vogue1A.01.046, Vogue1A.01.049, Vogue1A.01.054, Vogue1A.01.060, 
         Vogue1A.01.061, Vogue1A.01.063, Vogue1A.01.066, Vogue1A.01.067, 
         Vogue1A.01.072, Vogue1A.01.073, Vogue1A.01.077, Vogue1A.01.078, 
         Vogue1A.01.079, Vogue1A.01.082, Vogue1A.01.085, Vogue1A.01.089, 
         Vogue1A.01.090, Vogue1A.01.092, Vogue1A.01.095, Vogue1A.01.097, 
         Vogue1A.01.099, Vogue1A.01.100, Vogue1A.01.107, Vogue1A.01.112, 
         Vogue1A.01.113, Vogue1A.01.114, Vogue1A.01.118, Vogue1A.01.123, 
         Vogue1A.01.125, Vogue1A.01.126, Vogue1A.01.127, Vogue1A.01.128, 
         Vogue1A.01.130, Vogue1A.01.134, Vogue1A.01.135, Vogue1A.01.137, 
         Vogue1A.01.139, Vogue1A.01.140, Vogue1A.01.142, Vogue1A.01.145, 
         Vogue1A.01.146, Vogue1A.01.147, Vogue1A.01.150, Vogue1A.01.152, 
         Vogue1A.01.156, Vogue1A.01.157, Vogue1A.01.161, Vogue1A.01.162, 
         Vogue1A.01.163, Vogue1A.01.166, Vogue1A.01.169, Vogue1A.01.170, 
         Vogue1A.01.172, Vogue1A.01.173, Vogue1A.01.174, Vogue1A.01.175, 
         Vogue1A.01.184, Vogue1A.01.186, Vogue1A.01.187, Vogue1A.01.191, 
         Vogue1A.01.192, Vogue1A.01.197, Vogue1A.01.198, Vogue1A.01.200, 
         Vogue1A.01.201, Vogue1A.01.203, Vogue1A.01.205, Vogue1A.01.207, 
         Vogue1A.01.208, Vogue1A.01.211, Vogue1A.01.212, Vogue1A.01.213, 
         Vogue1A.01.214, Vogue1A.01.215, Vogue1A.01.216, Vogue1A.01.217, 
         Vogue1A.01.218, Vogue1A.01.219, Vogue1A.01.224, Vogue1A.01.227, 
         Vogue1A.01.231, Vogue1A.01.233, Vogue1A.01.236, Vogue1A.01.237, 
         Vogue1A.01.238)
