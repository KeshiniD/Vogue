#load dataset
total <- read.csv(file = "1B2metabac_condensedv2.csv")

#load packages
library(vegan)
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)
library(vcd)
library(effsize)

#convert variables into factors

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

#2X2 tables-Fishers (cat-cat)
#pvalue <0.05 reject null and there is assocation
#pvalue >0.05 do not reject null and there is no association
#effect size; 0.1 small, 0.3 medium, 0.5 large

#Demographics
a <- xtabs(~Age.cat + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~BMI.cat + CST , data = total)
fisher.test(a)
assocstats(a)

#BMI
a <- xtabs(~BMI.under.cat + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~BMI.over.cat + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~Ethnicity.cat + CST , data = total)
fisher.test(a)
assocstats(a)

#genital infections
a <- xtabs(~UTI.ever + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~Chlamydia.ever + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~Genwarts.ever + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~Trich.ever + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~GenHerpes.ever + CST , data = total)
fisher.test(a)
assocstats(a)

#meds
a <- xtabs(~Antimicrobial.Use..y.1..n.0. + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~X.Non..Prescription..y.1..n.0. + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~probiotics.2.months + CST, data = total)
fisher.test(a)
assocstats(a)

#symptoms
a <- xtabs(~Presence.Symptoms.2wks + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~Abnormal.discharge.2wks + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~Abnormal.odor.2wks + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~Irritation.Discomfort.2wks + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~Other.Symptoms.2wks + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~Presence.Symptoms.48hrs + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~Abnormal.discharge.48hrs + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~Abnormal.odor.48hrs + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~Irritation.Discomfort.48hrs + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~Other.Symptoms.48hrs + CST , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~Symptom.pain + CST , data = total)
fisher.test(a)
assocstats(a)

#Sexual Activity
a <- xtabs(~Vaginal.intercourse.in.past.48.hours..y.1..n.0. + CST , data = total)
fisher.test(a)
assocstats(a)
a <- xtabs(~Freq.oral.sex.cat + CST , data = total)
fisher.test(a)
assocstats(a)
a <- xtabs(~Freq.anal.sex.cat + CST , data = total)
fisher.test(a)
assocstats(a)
a <- xtabs(~Freq.sex.toy.use.cat + CST , data = total)
fisher.test(a)
assocstats(a)
a <- xtabs(~Sexual.Partners.cat + CST , data = total)
fisher.test(a)
assocstats(a)
a <- xtabs(~Number.partners.in.past.year.cat + CST , data = total)
fisher.test(a)
assocstats(a)
a <- xtabs(~contraception.H + CST , data = total)
fisher.test(a)
assocstats(a)
a <- xtabs(~contraception.B.M + CST , data = total)
fisher.test(a)
assocstats(a)
a <- xtabs(~contraception.C.IUD + CST , data = total)
fisher.test(a)
assocstats(a)
a <- xtabs(~Contraception.none + CST , data = total)
fisher.test(a)
assocstats(a)
a <- xtabs(~condoms.48h + CST , data = total)
fisher.test(a)
assocstats(a)
#Pregnancy
a <- xtabs(~Pregnancy.cat + CST , data = total)
fisher.test(a)
assocstats(a)
#Product use
a <- xtabs(~Feminine.products + CST , data = total)
fisher.test(a)
assocstats(a)
a <- xtabs(~Feminine.products.48hrs + CST , data = total)
fisher.test(a)
assocstats(a)
a <- xtabs(~Tampon.Use.cat + CST , data = total)
fisher.test(a)
assocstats(a)
a <- xtabs(~Tampon.use.1mth + CST , data = total)
fisher.test(a)
assocstats(a)
#substance use
a <- xtabs(~Substance.Use + CST , data = total)
fisher.test(a)
assocstats(a)
a <- xtabs(~smoking.current + CST , data = total)
fisher.test(a)
assocstats(a)

#one-way anova (cont-2+cat)
#pvalue <0.05 reject null and there is assocation
#pvalue >0.05 do not reject null and there is no association
summary(aov(total$BV..number.of.episodes.2.months. ~total$CST))
summary(aov(total$BV..number.of.episodes.year. ~total$CST))
summary(aov(total$BV..number.of.episodes.lifetime. ~total$CST))

summary(aov(total$Yeast..2months. ~total$CST))
summary(aov(total$Yeast..year. ~total$CST))
summary(aov(total$Yeast..lifetime. ~total$CST))

######################################################################
#Mar-5-16
#DM wants this cat, not episodes
total$Yeast.ever <- ifelse(total$Yeast..lifetime. > 0, 
                           c("1"), c("0")) 
#convert Yeast.ever from character into factor
total$Yeast.ever <- factor(total$Yeast.ever) 

a <- xtabs(~Yeast.ever + CST , data = total)
fisher.test(a)
assocstats(a)

####################################################################
#Mar-5-16
#summary per CST; nice list
aggregate(total$Contraception.none, list(total$CST), summary)
# look at only women with high Nugent scores
newdata <- subset(total, Nugent.score >= 4) 
levels(droplevels(total$CST)) # get rid of empty levels

###############
#dean.R has loops for fishers, anova for CST and t.test and correlation for SD
#need to remove CSTII before do loops for CST
total2 <- total[-c(1),]
total2$CST <- droplevels(total2$CST)
