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

######################################################################
#Mar-5-16
#DM wants this cat, not episodes
total$Yeast.ever <- ifelse(total$Yeast..lifetime. > 0, 
                           c("1"), c("0")) 
#convert Yeast.ever from character into factor
total$Yeast.ever <- factor(total$Yeast.ever) 
####################################################################3

#simple linear regression
#Shannon's Diversity
cor.test(~total$Shannon.s.Diversity + total$BV..number.of.episodes.2.months.,  
         method = c("pearson"))

cor.test(~total$Shannon.s.Diversity + total$BV..number.of.episodes.year.,  
         method = c("pearson"))

cor.test(~total$Shannon.s.Diversity + total$BV..number.of.episodes.lifetime.,  
         method = c("pearson"))

cor.test(~total$Shannon.s.Diversity + total$Yeast..2months.,  
         method = c("pearson"))

cor.test(~total$Shannon.s.Diversity + total$Yeast..year.,  
         method = c("pearson"))

cor.test(~total$Shannon.s.Diversity + total$Yeast..lifetime.,  
         method = c("pearson"))

#t.test
#Shannon's Diversity
#Demographics
t.test(Shannon.s.Diversity~Age.cat, data = total)
cohen.d(Shannon.s.Diversity~Age.cat, data = total)


t.test(Shannon.s.Diversity~BMI.under.cat, data = total)
cohen.d(Shannon.s.Diversity~BMI.under.cat, data = total)

t.test(Shannon.s.Diversity~BMI.over.cat, data = total)
cohen.d(Shannon.s.Diversity~BMI.over.cat, data = total)

t.test(Shannon.s.Diversity~Ethnicity.cat, data = total)
cohen.d(Shannon.s.Diversity~Ethnicity.cat, data = total)

#genital infections
t.test(Shannon.s.Diversity~Yeast.ever, data = total)
cohen.d(Shannon.s.Diversity~Yeast.ever, data = total)

t.test(Shannon.s.Diversity~UTI.ever, data = total)
cohen.d(Shannon.s.Diversity~UTI.ever, data = total)
t.test(Shannon.s.Diversity~Chlamydia.ever, data = total)
cohen.d(Shannon.s.Diversity~Chlamydia.ever, data = total)
t.test(Shannon.s.Diversity~Genwarts.ever, data = total)
cohen.d(Shannon.s.Diversity~Genwarts.ever, data = total)
t.test(Shannon.s.Diversity~Trich.ever, data = total)
cohen.d(Shannon.s.Diversity~Trich.ever, data = total)
t.test(Shannon.s.Diversity~GenHerpes.ever, data = total)
cohen.d(Shannon.s.Diversity~GenHerpes.ever, data = total)

#meds
t.test(Shannon.s.Diversity~Antimicrobial.Use..y.1..n.0., data = total)
cohen.d(Shannon.s.Diversity~Antimicrobial.Use..y.1..n.0., data = total)
t.test(Shannon.s.Diversity~X.Non..Prescription..y.1..n.0., data = total)
cohen.d(Shannon.s.Diversity~X.Non..Prescription..y.1..n.0., data = total)
t.test(Shannon.s.Diversity~probiotics.2.months, data = total)
cohen.d(Shannon.s.Diversity~probiotics.2.months, data = total)

#symptoms
t.test(Shannon.s.Diversity~Presence.Symptoms.2wks, data = total)
cohen.d(Shannon.s.Diversity~Presence.Symptoms.2wks, data = total)
t.test(Shannon.s.Diversity~Abnormal.discharge.2wks, data = total)
cohen.d(Shannon.s.Diversity~Abnormal.discharge.2wks, data = total)
t.test(Shannon.s.Diversity~Abnormal.odor.2wks, data = total)
cohen.d(Shannon.s.Diversity~Abnormal.odor.2wks, data = total)
t.test(Shannon.s.Diversity~Irritation.Discomfort.2wks, data = total)
cohen.d(Shannon.s.Diversity~Irritation.Discomfort.2wks, data = total)
t.test(Shannon.s.Diversity~Other.Symptoms.2wks, data = total)
cohen.d(Shannon.s.Diversity~Other.Symptoms.2wks, data = total)
t.test(Shannon.s.Diversity~Presence.Symptoms.48hrs, data = total)
cohen.d(Shannon.s.Diversity~Presence.Symptoms.48hrs, data = total)
t.test(Shannon.s.Diversity~Abnormal.discharge.48hrs, data = total)
cohen.d(Shannon.s.Diversity~Abnormal.discharge.48hrs, data = total)
t.test(Shannon.s.Diversity~Abnormal.odor.48hrs, data = total)
cohen.d(Shannon.s.Diversity~Abnormal.odor.48hrs, data = total)
t.test(Shannon.s.Diversity~Irritation.Discomfort.48hrs, data = total)
cohen.d(Shannon.s.Diversity~Irritation.Discomfort.48hrs, data = total)
t.test(Shannon.s.Diversity~Other.Symptoms.48hrs, data = total)
cohen.d(Shannon.s.Diversity~Other.Symptoms.48hrs, data = total)
t.test(Shannon.s.Diversity~Symptom.pain, data = total)
cohen.d(Shannon.s.Diversity~Symptom.pain, data = total)

#Sexual Activity
t.test(Shannon.s.Diversity~Vaginal.intercourse.in.past.48.hours..y.1..n.0., data = total)
cohen.d(Shannon.s.Diversity~Vaginal.intercourse.in.past.48.hours..y.1..n.0., data = total)
t.test(Shannon.s.Diversity~Freq.oral.sex.cat, data = total)
cohen.d(Shannon.s.Diversity~Freq.oral.sex.cat, data = total)
t.test(Shannon.s.Diversity~Freq.anal.sex.cat, data = total)
cohen.d(Shannon.s.Diversity~Freq.anal.sex.cat, data = total)
t.test(Shannon.s.Diversity~Freq.sex.toy.use.cat, data = total)
cohen.d(Shannon.s.Diversity~Freq.sex.toy.use.cat, data = total)
t.test(Shannon.s.Diversity~Sexual.Partners.cat, data = total)
cohen.d(Shannon.s.Diversity~Sexual.Partners.cat, data = total)
t.test(Shannon.s.Diversity~Number.partners.in.past.year.cat, data = total)
cohen.d(Shannon.s.Diversity~Number.partners.in.past.year.cat, data = total)

#Contraception

t.test(Shannon.s.Diversity~contraception.H, data = total)
cohen.d(Shannon.s.Diversity~contraception.H, data = total)

t.test(Shannon.s.Diversity~contraception.B.M, data = total)
cohen.d(Shannon.s.Diversity~contraception.B.M, data = total)

t.test(Shannon.s.Diversity~contraception.C.IUD, data = total)
cohen.d(Shannon.s.Diversity~contraception.C.IUD, data = total)

t.test(Shannon.s.Diversity~Contraception.none, data = total)
cohen.d(Shannon.s.Diversity~Contraception.none, data = total)

t.test(Shannon.s.Diversity~condoms.48h, data = total)
cohen.d(Shannon.s.Diversity~condoms.48h, data = total)

#Pregnancy
t.test(Shannon.s.Diversity~Pregnancy.cat, data = total)
cohen.d(Shannon.s.Diversity~Pregnancy.cat, data = total)

#Product use
t.test(Shannon.s.Diversity~Feminine.products, data = total)
cohen.d(Shannon.s.Diversity~Feminine.products, data = total)
t.test(Shannon.s.Diversity~Feminine.products.48hrs, data = total)
cohen.d(Shannon.s.Diversity~Feminine.products.48hrs, data = total)
t.test(Shannon.s.Diversity~Tampon.Use.cat, data = total)
cohen.d(Shannon.s.Diversity~Tampon.Use.cat, data = total)
t.test(Shannon.s.Diversity~Tampon.use.1mth, data = total)
cohen.d(Shannon.s.Diversity~Tampon.use.1mth, data = total)

#substance use
t.test(Shannon.s.Diversity~Substance.Use, data = total)
cohen.d(Shannon.s.Diversity~Substance.Use, data = total)
t.test(Shannon.s.Diversity~smoking.current, data = total)
cohen.d(Shannon.s.Diversity~smoking.current, data = total)

#adjust p-values; Benjamini Hochburg
pvals <- read.csv(file = "clipboard") #copied from excel
pvals

a <- p.adjust(pvals$x, method = 'hochberg', n = 26) #n=number of test done (total pvals)
View(a)

###########################################################
#Mar-5-16
#summary per CST; nice list
aggregate(total$Contraception.none, list(total$CST), summary)
# look at only women with high Nugent scores
newdata <- subset(total, Nugent.score >= 4) 
levels(droplevels(total$CST)) # get rid of empty levels
#trying anlayses without low Nugent women