#load packages
library(vegan)
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)
library(assertthat)
library(entropart)
library(epitools)
library(ggtree)
library(PredictABEL) #for adjusted odds ratio

#call for entire 1B2 data
total <- read.csv(file.path("1B2metbac_v2.csv"))

#Contingency Tables
#2x2contingency table
a <- xtabs(~Nugent.score + Ethnicity , data = total)
kable(a)
#3x3 table
a <- xtabs(~Nugent.score + Ethnicity + Marital.Status , data = total)
a <- as.data.frame(a)

#Another way
attach(total)
#can just give names, since used attach()
mytable <- table(total$Ethnicity.cat,total$BMI.cat) #has to be same lengths
mytable
#works with 0,1, factors, and integers and numerics

#different table stats
mytable <- table(total$Abnormal.discharge.2wks, total$Nugent.score) 
mytable
margin.table(mytable, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(mytable, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) #column percentages

# for 3-way table
mytable <- table(total$Abnormal.discharge.2wks, total$Nugent.score, 
                 total$Freq.of.Menstrual.Period) 
ftable(mytable)
summary(mytable) #chi-squared
#also works for 2x2 tables
#table() ignored NA values, if want use exclude=NULL and works

#Fisher's and Chisquared
#can only use both for 2x2 matrix
chisq.test(mytable) # will use fishers instead due to small sample size
fisher.test(mytable) #does not work for 3x3
mantelhaen.test(mytable) #only use when counts greater than 1 in each cell, will not use this

#Use above code to create 2x2 tables and use in fisher.test() to assess associations
#will use for nugent score, cst, and amsels to relate them to remaining variables
#compare findings with odds ratio