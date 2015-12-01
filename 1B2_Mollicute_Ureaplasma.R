#load packages
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

#load dataset
data <- read.csv(file.path("1B2_Mollicute_Ureaplasma.csv"))

#make variables into factors
data$mollicutes <- factor(data$mollicutes)
data$ureaplasma <- factor(data$ureaplasma)

#summary of each variables
summary(data$mollicutes)
summary(data$ureaplasma)

#Mollicutes 
#1-positive: 18
#2-negative: 6
#3-not tested: 2

#Ureaplasma
#1-negative: 5
#2-parvum: 14
#3-urealyticum: 5
#4-parvum&urealyticum: 0
#5-not tested: 2


