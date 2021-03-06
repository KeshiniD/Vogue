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
total <- read.csv(file.path("1B2metabac_condensed.csv"))
#remove random columns
total$X  <-  NULL
total$X.1  <-  NULL
total$X.2  <-  NULL

#need to separate SD into cat; sep based on mean
mean(total$Shannon.s.Diversity)
summary(total$Shannon.s.Diversity)

#create new cat
total$SD.cat[total$Shannon.s.Diversity < mean(total$Shannon.s.Diversity)] <- "0" 
total$SD.cat[total$Shannon.s.Diversity > mean(total$Shannon.s.Diversity)] <- "1" 

#convert SD.cat from character into factor
total$SD.cat <- factor(total$SD.cat)  

#need to be factors if wish to treat like categories
total$Nugent.score.cat <- factor(total$Nugent.score.cat)
total$Amsels.cat <- factor(total$Amsels.cat)
total$Age.cat <- factor(total$Age.cat)
total$BMI.cat <- factor(total$BMI.cat)
total$Ethnicity.cat <- factor(total$Ethnicity.cat)
total$Antimicrobial.Use..y.1..n.0. <- factor(total$Antimicrobial.Use..y.1..n.0.)
total$X.Non..Prescription..y.1..n.0. <- factor(total$X.Non..Prescription..y.1..n.0.)
total$Tampon.Use.cat <- factor(total$Tampon.Use.cat)
total$Vaginal.intercourse.in.past.48.hours..y.1..n.0. <- factor(total$Vaginal.intercourse.in.past.48.hours..y.1..n.0.)
total$Presence.Symptoms.2wks <- factor(total$Presence.Symptoms.2wks)
total$Abnormal.discharge.2wks <- factor(total$Abnormal.discharge.2wks)
total$Abnormal.odor.2wks <- factor(total$Abnormal.odor.2wks)
total$Irritation.Discomfort.2wks <- factor(total$Irritation.Discomfort.2wks)
total$Other.Symptoms.2wks <- factor(total$Other.Symptoms.2wks)
total$Presence.Symptoms.48hrs <- factor(total$Presence.Symptoms.48hrs)
total$Abnormal.discharge.48hrs <- factor(total$Abnormal.discharge.48hrs)
total$Abnormal.odor.48hrs <- factor(total$Abnormal.odor.48hrs)
total$Irritation.Discomfort.48hrs <- factor(total$Irritation.Discomfort.48hrs)
total$Other.Symptoms.48hrs <- factor(total$Other.Symptoms.48hrs)
total$contraception.H <- factor(total$contraception.H)
total$contraception.B.M <- factor(total$contraception.B.M)
total$contraception.C.IUD <- factor(total$contraception.C.IUD)
total$condoms.48h <- factor(total$condoms.48h)
total$probiotics.2.months <- factor(total$probiotics.2.months)
total$CST <- factor(total$CST)
total$CSTI <- factor(total$CSTI)
total$CSTII <- factor(total$CSTII)
total$CSTIII <- factor(total$CSTIII)
total$CSTIVA <- factor(total$CSTIVA)
total$CSTIVC <- factor(total$CSTIVC)
total$CSTIVD <- factor(total$CSTIVD)
total$Sexual.Partners.cat <- factor(total$Sexual.Partners.cat)
total$Freq.oral.sex.cat <- factor(total$Freq.oral.sex.cat)
total$Freq.anal.sex.cat <- factor(total$Freq.anal.sex.cat)
total$Freq.sex.toy.use.cat <- factor(total$Freq.sex.toy.use.cat)
total$Chlamydia.ever <- factor(total$Chlamydia.ever)
total$Genwarts.ever <- factor(total$Genwarts.ever)
total$Number.partners.in.past.year.cat <- factor(total$Number.partners.in.past.year.cat)
total$UTI.ever <- factor(total$UTI.ever)
total$Trich.ever <- factor(total$Trich.ever)
total$GenHerpes.ever <- factor(total$GenHerpes.ever)
total$Pregnancy.cat <- factor(total$Pregnancy.cat)
total$Feminine.products <- factor(total$Feminine.products)
total$Feminine.products.48hrs <- factor(total$Feminine.products.48hrs)
total$Substance.Use <- factor(total$Substance.Use)
total$smoking.current <- factor(total$smoking.current)
total$Symptom.pain <- factor(total$Symptom.pain)
total$Contraception.none <- factor(total$Contraception.none)
total$Tampon.use.1mth <- factor(total$Tampon.use.1mth)

#2X2 tables
#Demographics
a <- xtabs(~SD.cat + Age.cat , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + BMI.cat , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Ethnicity.cat , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

#episodes of BV
a <- xtabs(~SD.cat + BV..number.of.episodes.2.months. , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + BV..number.of.episodes.year. , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + BV..number.of.episodes.lifetime. , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

#episodes of yeast
a <- xtabs(~SD.cat + Yeast..2months. , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Yeast..year. , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Yeast..lifetime. , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

#genital infections
a <- xtabs(~SD.cat + UTI.ever , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Chlamydia.ever , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Genwarts.ever , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Trich.ever , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + GenHerpes.ever , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

#meds
a <- xtabs(~SD.cat + Antimicrobial.Use..y.1..n.0. , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + X.Non..Prescription..y.1..n.0. , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + probiotics.2.months , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

#symptoms
a <- xtabs(~SD.cat + Presence.Symptoms.2wks , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Abnormal.discharge.2wks , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Abnormal.odor.2wks , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Irritation.Discomfort.2wks , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Other.Symptoms.2wks , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Presence.Symptoms.48hrs , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Abnormal.discharge.48hrs , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Abnormal.odor.48hrs , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Irritation.Discomfort.48hrs , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Other.Symptoms.48hrs , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Symptom.pain , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

#Sexual Activity
a <- xtabs(~SD.cat + Vaginal.intercourse.in.past.48.hours..y.1..n.0. , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Freq.oral.sex.cat , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Freq.anal.sex.cat , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Freq.sex.toy.use.cat , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Sexual.Partners.cat , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Number.partners.in.past.year.cat , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + contraception.H , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + contraception.B.M , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + contraception.C.IUD , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Contraception.none , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + condoms.48h , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

#Pregnancy
a <- xtabs(~SD.cat + Pregnancy.cat , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

#Product use
a <- xtabs(~SD.cat + Feminine.products , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Feminine.products.48hrs , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Tampon.Use.cat , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + Tampon.use.1mth , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

#substance use
a <- xtabs(~SD.cat + Substance.Use , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)

a <- xtabs(~SD.cat + smoking.current , data = total)
a <- as.data.frame(a)
margin.table(a, 1) # A frequencies [(summed over B) AD over nugent]
margin.table(a, 2) # B frequencies [(summed over A) nugent over AD]
prop.table(a) # cell percentages
prop.table(a, 1) # row percentages 
prop.table(a, 2) #column percentages
summary(a)