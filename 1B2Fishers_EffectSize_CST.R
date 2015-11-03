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
library(vcd)

#call for entire 1B2 data
total <- read.csv(file.path("1B2metabac_condensed.csv"))
#remove random columns
total$X  <-  NULL
total$X.1  <-  NULL
total$X.2  <-  NULL

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

#2X2 tables-Fishers-Effect Size
#Interested in Phi
#CSTI
#Demographics
a <- xtabs(~CSTI + Age.cat , data = total)
assocstats(a)

a <- xtabs(~CSTI + BMI.cat , data = total)
assocstats(a)

a <- xtabs(~CSTI + Ethnicity.cat , data = total)
assocstats(a)

#episodes of BV
a <- xtabs(~CSTI + BV..number.of.episodes.2.months. , data = total)
assocstats(a)

a <- xtabs(~CSTI + BV..number.of.episodes.year. , data = total)
assocstats(a)

a <- xtabs(~CSTI + BV..number.of.episodes.lifetime. , data = total)
assocstats(a)

#episodes of yeast
a <- xtabs(~CSTI + Yeast..2months. , data = total)
assocstats(a)

a <- xtabs(~CSTI + Yeast..year. , data = total)
assocstats(a)

a <- xtabs(~CSTI + Yeast..lifetime. , data = total)
assocstats(a)

#genital infections
a <- xtabs(~CSTI + UTI.ever , data = total)
assocstats(a)

a <- xtabs(~CSTI + Chlamydia.ever , data = total)
assocstats(a)

a <- xtabs(~CSTI + Genwarts.ever , data = total)
assocstats(a)

a <- xtabs(~CSTI + Trich.ever , data = total)
assocstats(a)

a <- xtabs(~CSTI + GenHerpes.ever , data = total)
assocstats(a)

#meds
a <- xtabs(~CSTI + Antimicrobial.Use..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTI + X.Non..Prescription..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTI + probiotics.2.months , data = total)
assocstats(a)

#symptoms
a <- xtabs(~CSTI + Presence.Symptoms.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTI + Abnormal.discharge.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTI + Abnormal.odor.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTI + Irritation.Discomfort.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTI + Other.Symptoms.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTI + Presence.Symptoms.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTI + Abnormal.discharge.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTI + Abnormal.odor.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTI + Irritation.Discomfort.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTI + Other.Symptoms.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTI + Symptom.pain , data = total)
assocstats(a)

#Sexual Activity
a <- xtabs(~CSTI + Vaginal.intercourse.in.past.48.hours..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTI + Freq.oral.sex.cat , data = total)
assocstats(a)

a <- xtabs(~CSTI + Freq.anal.sex.cat , data = total)
assocstats(a)

a <- xtabs(~CSTI + Freq.sex.toy.use.cat , data = total)
assocstats(a)

a <- xtabs(~CSTI + Sexual.Partners.cat , data = total)
assocstats(a)

a <- xtabs(~CSTI + Number.partners.in.past.year.cat , data = total)
assocstats(a)

a <- xtabs(~CSTI + contraception.H , data = total)
assocstats(a)

a <- xtabs(~CSTI + contraception.B.M , data = total)
assocstats(a)

a <- xtabs(~CSTI + contraception.C.IUD , data = total)
assocstats(a)

a <- xtabs(~CSTI + Contraception.none , data = total)
assocstats(a)

a <- xtabs(~CSTI + condoms.48h , data = total)
assocstats(a)

#Pregnancy
a <- xtabs(~CSTI + Pregnancy.cat , data = total)
assocstats(a)

#Product use
a <- xtabs(~CSTI + Feminine.products , data = total)
assocstats(a)

a <- xtabs(~CSTI + Feminine.products.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTI + Tampon.Use.cat , data = total)
assocstats(a)

a <- xtabs(~CSTI + Tampon.use.1mth , data = total)
assocstats(a)

#substance use
a <- xtabs(~CSTI + Substance.Use , data = total)
assocstats(a)

a <- xtabs(~CSTI + smoking.current , data = total)
assocstats(a)

#CSTII
#Demographics
a <- xtabs(~CSTII + Age.cat , data = total)
assocstats(a)

a <- xtabs(~CSTII + BMI.cat , data = total)
assocstats(a)

a <- xtabs(~CSTII + Ethnicity.cat , data = total)
assocstats(a)

#episodes of BV
a <- xtabs(~CSTII + BV..number.of.episodes.2.months. , data = total)
assocstats(a)

a <- xtabs(~CSTII + BV..number.of.episodes.year. , data = total)
assocstats(a)

a <- xtabs(~CSTII + BV..number.of.episodes.lifetime. , data = total)
assocstats(a)

#episodes of yeast
a <- xtabs(~CSTII + Yeast..2months. , data = total)
assocstats(a)

a <- xtabs(~CSTII + Yeast..year. , data = total)
assocstats(a)

a <- xtabs(~CSTII + Yeast..lifetime. , data = total)
assocstats(a)

#genital infections
a <- xtabs(~CSTII + UTI.ever , data = total)
assocstats(a)

a <- xtabs(~CSTII + Chlamydia.ever , data = total)
assocstats(a)

a <- xtabs(~CSTII + Genwarts.ever , data = total)
assocstats(a)

a <- xtabs(~CSTII + Trich.ever , data = total)
assocstats(a)

a <- xtabs(~CSTII + GenHerpes.ever , data = total)
assocstats(a)

#meds
a <- xtabs(~CSTII + Antimicrobial.Use..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTII + X.Non..Prescription..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTII + probiotics.2.months , data = total)
assocstats(a)

#symptoms
a <- xtabs(~CSTII + Presence.Symptoms.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTII + Abnormal.discharge.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTII + Abnormal.odor.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTII + Irritation.Discomfort.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTII + Other.Symptoms.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTII + Presence.Symptoms.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTII + Abnormal.discharge.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTII + Abnormal.odor.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTII + Irritation.Discomfort.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTII + Other.Symptoms.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTII + Symptom.pain , data = total)
assocstats(a)

#Sexual Activity
a <- xtabs(~CSTII + Vaginal.intercourse.in.past.48.hours..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTII + Freq.oral.sex.cat , data = total)
assocstats(a)

a <- xtabs(~CSTII + Freq.anal.sex.cat , data = total)
assocstats(a)

a <- xtabs(~CSTII + Freq.sex.toy.use.cat , data = total)
assocstats(a)

a <- xtabs(~CSTII + Sexual.Partners.cat , data = total)
assocstats(a)

a <- xtabs(~CSTII + Number.partners.in.past.year.cat , data = total)
assocstats(a)

a <- xtabs(~CSTII + contraception.H , data = total)
assocstats(a)

a <- xtabs(~CSTII + contraception.B.M , data = total)
assocstats(a)

a <- xtabs(~CSTII + contraception.C.IUD , data = total)
assocstats(a)

a <- xtabs(~CSTII + Contraception.none , data = total)
assocstats(a)

a <- xtabs(~CSTII + condoms.48h , data = total)
assocstats(a)

#Pregnancy
a <- xtabs(~CSTII + Pregnancy.cat , data = total)
assocstats(a)

#Product use
a <- xtabs(~CSTII + Feminine.products , data = total)
assocstats(a)

a <- xtabs(~CSTII + Feminine.products.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTII + Tampon.Use.cat , data = total)
assocstats(a)

a <- xtabs(~CSTII + Tampon.use.1mth , data = total)
assocstats(a)

#substance use
a <- xtabs(~CSTII + Substance.Use , data = total)
assocstats(a)

a <- xtabs(~CSTII + smoking.current , data = total)
assocstats(a)

#CSTIII
#Demographics
a <- xtabs(~CSTIII + Age.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIII + BMI.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Ethnicity.cat , data = total)
assocstats(a)

#episodes of BV
a <- xtabs(~CSTIII + BV..number.of.episodes.2.months. , data = total)
assocstats(a)

a <- xtabs(~CSTIII + BV..number.of.episodes.year. , data = total)
assocstats(a)

a <- xtabs(~CSTIII + BV..number.of.episodes.lifetime. , data = total)
assocstats(a)

#episodes of yeast
a <- xtabs(~CSTIII + Yeast..2months. , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Yeast..year. , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Yeast..lifetime. , data = total)
assocstats(a)

#genital infections
a <- xtabs(~CSTIII + UTI.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Chlamydia.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Genwarts.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Trich.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIII + GenHerpes.ever , data = total)
assocstats(a)

#meds
a <- xtabs(~CSTIII + Antimicrobial.Use..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTIII + X.Non..Prescription..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTIII + probiotics.2.months , data = total)
assocstats(a)

#symptoms
a <- xtabs(~CSTIII + Presence.Symptoms.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Abnormal.discharge.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Abnormal.odor.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Irritation.Discomfort.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Other.Symptoms.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Presence.Symptoms.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Abnormal.discharge.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Abnormal.odor.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Irritation.Discomfort.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Other.Symptoms.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Symptom.pain , data = total)
assocstats(a)

#Sexual Activity
a <- xtabs(~CSTIII + Vaginal.intercourse.in.past.48.hours..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Freq.oral.sex.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Freq.anal.sex.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Freq.sex.toy.use.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Sexual.Partners.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Number.partners.in.past.year.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIII + contraception.H , data = total)
assocstats(a)

a <- xtabs(~CSTIII + contraception.B.M , data = total)
assocstats(a)

a <- xtabs(~CSTIII + contraception.C.IUD , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Contraception.none , data = total)
assocstats(a)

a <- xtabs(~CSTIII + condoms.48h , data = total)
assocstats(a)

#Pregnancy
a <- xtabs(~CSTIII + Pregnancy.cat , data = total)
assocstats(a)

#Product use
a <- xtabs(~CSTIII + Feminine.products , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Feminine.products.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Tampon.Use.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIII + Tampon.use.1mth , data = total)
assocstats(a)

#substance use
a <- xtabs(~CSTIII + Substance.Use , data = total)
assocstats(a)

a <- xtabs(~CSTIII + smoking.current , data = total)
assocstats(a)

#CSTIVA
#Demographics
a <- xtabs(~CSTIVA + Age.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + BMI.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Ethnicity.cat , data = total)
assocstats(a)

#episodes of BV
a <- xtabs(~CSTIVA + BV..number.of.episodes.2.months. , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + BV..number.of.episodes.year. , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + BV..number.of.episodes.lifetime. , data = total)
assocstats(a)

#episodes of yeast
a <- xtabs(~CSTIVA + Yeast..2months. , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Yeast..year. , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Yeast..lifetime. , data = total)
assocstats(a)

#genital infections
a <- xtabs(~CSTIVA + UTI.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Chlamydia.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Genwarts.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Trich.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + GenHerpes.ever , data = total)
assocstats(a)

#meds
a <- xtabs(~CSTIVA + Antimicrobial.Use..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + X.Non..Prescription..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + probiotics.2.months , data = total)
assocstats(a)

#symptoms
a <- xtabs(~CSTIVA + Presence.Symptoms.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Abnormal.discharge.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Abnormal.odor.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Irritation.Discomfort.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Other.Symptoms.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Presence.Symptoms.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Abnormal.discharge.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Abnormal.odor.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Irritation.Discomfort.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Other.Symptoms.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Symptom.pain , data = total)
assocstats(a)

#Sexual Activity
a <- xtabs(~CSTIVA + Vaginal.intercourse.in.past.48.hours..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Freq.oral.sex.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Freq.anal.sex.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Freq.sex.toy.use.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Sexual.Partners.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Number.partners.in.past.year.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + contraception.H , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + contraception.B.M , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + contraception.C.IUD , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Contraception.none , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + condoms.48h , data = total)
assocstats(a)

#Pregnancy
a <- xtabs(~CSTIVA + Pregnancy.cat , data = total)
assocstats(a)

#Product use
a <- xtabs(~CSTIVA + Feminine.products , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Feminine.products.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Tampon.Use.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + Tampon.use.1mth , data = total)
assocstats(a)

#substance use
a <- xtabs(~CSTIVA + Substance.Use , data = total)
assocstats(a)

a <- xtabs(~CSTIVA + smoking.current , data = total)
assocstats(a)

#CSTIVC
#Demographics
a <- xtabs(~CSTIVC + Age.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + BMI.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Ethnicity.cat , data = total)
assocstats(a)

#episodes of BV
a <- xtabs(~CSTIVC + BV..number.of.episodes.2.months. , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + BV..number.of.episodes.year. , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + BV..number.of.episodes.lifetime. , data = total)
assocstats(a)

#episodes of yeast
a <- xtabs(~CSTIVC + Yeast..2months. , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Yeast..year. , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Yeast..lifetime. , data = total)
assocstats(a)

#genital infections
a <- xtabs(~CSTIVC + UTI.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Chlamydia.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Genwarts.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Trich.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + GenHerpes.ever , data = total)
assocstats(a)

#meds
a <- xtabs(~CSTIVC + Antimicrobial.Use..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + X.Non..Prescription..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + probiotics.2.months , data = total)
assocstats(a)

#symptoms
a <- xtabs(~CSTIVC + Presence.Symptoms.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Abnormal.discharge.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Abnormal.odor.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Irritation.Discomfort.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Other.Symptoms.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Presence.Symptoms.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Abnormal.discharge.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Abnormal.odor.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Irritation.Discomfort.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Other.Symptoms.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Symptom.pain , data = total)
assocstats(a)

#Sexual Activity
a <- xtabs(~CSTIVC + Vaginal.intercourse.in.past.48.hours..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Freq.oral.sex.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Freq.anal.sex.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Freq.sex.toy.use.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Sexual.Partners.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Number.partners.in.past.year.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + contraception.H , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + contraception.B.M , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + contraception.C.IUD , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Contraception.none , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + condoms.48h , data = total)
assocstats(a)

#Pregnancy
a <- xtabs(~CSTIVC + Pregnancy.cat , data = total)
assocstats(a)

#Product use
a <- xtabs(~CSTIVC + Feminine.products , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Feminine.products.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Tampon.Use.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + Tampon.use.1mth , data = total)
assocstats(a)

#substance use
a <- xtabs(~CSTIVC + Substance.Use , data = total)
assocstats(a)

a <- xtabs(~CSTIVC + smoking.current , data = total)
assocstats(a)

#CSTIVD
#Demographics
a <- xtabs(~CSTIVD + Age.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + BMI.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Ethnicity.cat , data = total)
assocstats(a)

#episodes of BV
a <- xtabs(~CSTIVD + BV..number.of.episodes.2.months. , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + BV..number.of.episodes.year. , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + BV..number.of.episodes.lifetime. , data = total)
assocstats(a)

#episodes of yeast
a <- xtabs(~CSTIVD + Yeast..2months. , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Yeast..year. , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Yeast..lifetime. , data = total)
assocstats(a)

#genital infections
a <- xtabs(~CSTIVD + UTI.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Chlamydia.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Genwarts.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Trich.ever , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + GenHerpes.ever , data = total)
assocstats(a)

#meds
a <- xtabs(~CSTIVD + Antimicrobial.Use..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + X.Non..Prescription..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + probiotics.2.months , data = total)
assocstats(a)

#symptoms
a <- xtabs(~CSTIVD + Presence.Symptoms.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Abnormal.discharge.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Abnormal.odor.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Irritation.Discomfort.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Other.Symptoms.2wks , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Presence.Symptoms.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Abnormal.discharge.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Abnormal.odor.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Irritation.Discomfort.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Other.Symptoms.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Symptom.pain , data = total)
assocstats(a)

#Sexual Activity
a <- xtabs(~CSTIVD + Vaginal.intercourse.in.past.48.hours..y.1..n.0. , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Freq.oral.sex.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Freq.anal.sex.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Freq.sex.toy.use.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Sexual.Partners.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Number.partners.in.past.year.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + contraception.H , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + contraception.B.M , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + contraception.C.IUD , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Contraception.none , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + condoms.48h , data = total)
assocstats(a)

#Pregnancy
a <- xtabs(~CSTIVD + Pregnancy.cat , data = total)
assocstats(a)

#Product use
a <- xtabs(~CSTIVD + Feminine.products , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Feminine.products.48hrs , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Tampon.Use.cat , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + Tampon.use.1mth , data = total)
assocstats(a)

#substance use
a <- xtabs(~CSTIVD + Substance.Use , data = total)
assocstats(a)

a <- xtabs(~CSTIVD + smoking.current , data = total)
assocstats(a)