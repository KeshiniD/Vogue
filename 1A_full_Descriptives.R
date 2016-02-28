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
data <- read.csv(file.path("VOGUE_1A.csv"))

#selecting participants included (omit those excluded)
### DEAN ADDED THIS
#does what the multiple lines below does so deleted them
dean <- read.csv("../Vogue/1A.csv")
nums <- substring(dean$Study.ID, 12)
ids <- paste0("Vogue 1A 01-", nums)
total <- data[which(data$study_id %in% ids), ]
###

#took groups from 1A_Subset_Analyses_metadata.R
#and wrote into this file
#rewrite to file
#write.csv(total3, "1A_full_grouped.csv")
###

#load dataset
total <- read.csv(file="1A_full_grouped.csv")

#make cats into factors
total$Age.cat <- factor(total$Age.cat)
total$BMI.under.cat <- factor(total$BMI.under.cat)
total$BMI.over.cat <- factor(total$BMI.over.cat)
total$Ethnicity.cat <- factor(total$Ethnicity.cat)
total$maritalstatus <- factor(total$maritalstatus)
total$educationlevel <- factor(total$educationlevel)
total$Pregnancy.cat <- factor(total$Pregnancy.cat)
total$contramethnone___1 <- factor(total$contramethnone___1)
total$contramethhormonal___1 <- factor(total$contramethhormonal___1)
total$contramethbarriermc___1 <- factor(total$contramethbarriermc___1)
total$contramethnotactive___1 <- factor(total$contramethnotactive___1)
total$abnormalodor2wk <- factor(total$abnormalodor2wk)
total$abnormaldischarge2wk <- factor(total$abnormaldischarge2wk)
total$irritationdiscomfort2wk <- factor(total$irritationdiscomfort2wk)
total$vagintercoursediscomfort <- factor(total$vagintercoursediscomfort)
total$vaginalsymptomother2wk <- factor(total$vaginalsymptomother2wk)
total$antimicrodrug <- factor(total$antimicrodrug)
total$rxdrug <- factor(total$rxdrug)
total$medical_condition <- factor(total$medical_condition)
total$menstrualcycle <- factor(total$menstrualcycle)
total$tamponusage <- factor(total$tamponusage)
total$doucheproducts <- factor(total$doucheproducts)
total$deodorantproducts <- factor(total$deodorantproducts)
total$substance_use_yn <- factor(total$substance_use_yn)
total$alcoholcurrent <- factor(total$alcoholcurrent)
total$tobaccouse <- factor(total$tobaccouse)
total$sexpartner <- factor(total$sexpartner)
total$vaginalintercourse48hr <- factor(total$vaginalintercourse48hr)
total$oralsxfrequency <- factor(total$oralsxfrequency)
total$analsxfrequency <- factor(total$analsxfrequency)
total$sextoyfrequency <- factor(total$sextoyfrequency)
total$UTI.ever <- factor(total$uti_infect)
total$Trich.ever <- factor(total$trich_infect) 
total$Condyloma.ever <- factor(total$condy_infect) 
total$GenHerpes.ever <- factor(total$herpes_infect) 
total$Chlamydia.ever <- factor(total$chlam_infect) 
total$Gonorrhea.ever <- factor(total$gonor_infect) 
total$Syphillis.ever <- factor(total$syph_infect)
total$nugent_score_result <- factor(total$nugent_score_result)
total$CST <- factor(total$CST)

#Descriptives
mean(total$uti_life, na.rm=TRUE)
sd(total$uti_life, na.rm=TRUE)
range(total$uti_life, na.rm=TRUE)
summary(factor(total$contramethbarrieroth___1))
summary(factor(total$Symptom.pain))
summary(factor(total$ethother___1))

#########################################################################
#merge 1A and 1B2 together to compare groups
a <- read.csv(file="1B2metabac_condensedv2.csv")
b <- read.csv(file="1A_full_grouped.csv")
#include study arm cat
a[,"study_arm"]  <- c("1B2")
b[,"study_arm"]  <- c("1A")

#fix up columns in 1A
#BV episodes
summary(b$bv_infecttotal_2mo)
summary(b$bv_infect)
b$bv_infect <- factor(b$bv_infect)
#bv_infect, if 2 then within 2mths, 1yr and life, episodes are 0
b$bv_infecttotal_2mo[b$bv_infect=='2'] <- '0'
b$bv_infecttotal_1yr[b$bv_infect=='2'] <- '0'                              
b$bv_life[b$bv_infect=='2'] <- '0'              

#Yeast episodes
b$yeast_infect <- factor(b$yeast_infect)
#yeast_infect, if 2 then within 2mths, 1yr and life, episodes are 0
b$yeast_infecttotal_2mo[b$yeast_infect=='2'] <- '0'
b$yeast_infecttotal_1yr[b$yeast_infect=='2'] <- '0'                              
b$yeast_life[b$yeast_infect=='2'] <- '0'  

#Symptoms
b$abnormaldischarge2wk <- factor(b$abnormaldischarge2wk)
b$abnormalodor2wk <- factor(b$abnormalodor2wk)
b$irritationdiscomfort2wk <- factor(b$irritationdiscomfort2wk)
b$vaginalsymptomother2wk <- factor(b$vaginalsymptomother2wk)

#abnormal symptoms none in 2 weeks, then none in 48hrs
b$abnormaldischarge48[b$abnormaldischarge2wk=='0'] <- '0' 
b$abnormalodor48[b$abnormalodor2wk=='0'] <- '0' 
b$irritationdiscomfort48[b$irritationdiscomfort2wk=='0'] <- '0' 
b$vaginalsymptomother48[b$vaginalsymptomother2wk=='0'] <- '0' 

#fix presence of symptoms
b$abnormaldischarge48 <- factor(b$abnormaldischarge48)
b$abnormalodor48 <- factor(b$abnormalodor48)
b$irritationdiscomfort48 <- factor(b$irritationdiscomfort48)
b$vaginalsymptomother48 <- factor(b$vaginalsymptomother48)

b$Presence.Symptoms.48hrs <- ifelse(b$abnormaldischarge48=='1'
                                    | b$abnormalodor48 == '1'
                                    |b$irritationdiscomfort48 == '1'
                                    | b$vaginalsymptomother48 == '1', 
                                    c("1"), c("0")) 

#rename variables in 1A to match 1B2
b2 <- dplyr::rename(b, Participants = study_id, Nugent.score =  nugent_score, 
                   Age = age, BMI = bmi, 
                   BV..number.of.episodes.2.months. = bv_infecttotal_2mo, 
                   BV..number.of.episodes.year. = bv_infecttotal_1yr, 
                   BV..number.of.episodes.lifetime. = bv_life, 
                   Yeast..2months. = yeast_infecttotal_2mo, 
                   Yeast..year. = yeast_infecttotal_1yr, 
                   Yeast..lifetime. = yeast_life, 
                   Antimicrobial.Use..y.1..n.0. = antimicrodrug, 
                   X.Non..Prescription..y.1..n.0. = rxdrug,
                   Vaginal.intercourse.in.past.48.hours..y.1..n.0. = vaginalintercourse48hr, 
                   Freq.oral.sex.cat = oralsxfrequency.cat, 
                   Freq.anal.sex.cat = analsxfrequency.cat, 
                   Freq.sex.toy.use.cat = sextoyfrequency.cat, 
                   Abnormal.discharge.2wks = abnormaldischarge2wk, 
                   Abnormal.odor.2wks = abnormalodor2wk, 
                   Irritation.Discomfort.2wks = irritationdiscomfort2wk, 
                   Other.Symptoms.2wks = vaginalsymptomother2wk, 
                   Abnormal.discharge.48hrs = abnormaldischarge48, 
                   Abnormal.odor.48hrs = abnormalodor48, 
                   Irritation.Discomfort.48hrs = irritationdiscomfort48, 
                   Other.Symptoms.48hrs = vaginalsymptomother48, 
                   Genwarts.ever = Condyloma.ever, 
                   Number.partners.in.past.year.cat = sexpartner1yr, 
                   contraception.H = Contraception.H, 
                   contraception.B.M = Contraception.B.M, 
                   contraception.C.IUD = Contraception.IUD, 
                   Substance.Use = substance_use_yn)

#subset 1A dataset
b3 <- b2 %>%
  select (Participants, Nugent.score, Age, BMI, 
          BV..number.of.episodes.2.months., BV..number.of.episodes.year., 
          BV..number.of.episodes.lifetime., Yeast..2months., 
          Yeast..year., Yeast..lifetime., Antimicrobial.Use..y.1..n.0., 
          X.Non..Prescription..y.1..n.0.,
          Vaginal.intercourse.in.past.48.hours..y.1..n.0., 
          Freq.oral.sex.cat, Freq.anal.sex.cat, Freq.sex.toy.use.cat, 
          Abnormal.discharge.2wks, Abnormal.odor.2wks, 
          Irritation.Discomfort.2wks, Other.Symptoms.2wks, 
          Abnormal.discharge.48hrs, Abnormal.odor.48hrs, 
          Irritation.Discomfort.48hrs, Other.Symptoms.48hrs, 
          Genwarts.ever, Number.partners.in.past.year.cat, 
          contraception.H, contraception.B.M, contraception.C.IUD, 
          Substance.Use, study_arm, Ethnicity.cat, Tampon.Use.cat, 
          Presence.Symptoms.2wks, Presence.Symptoms.48hrs, 
          Chlamydia.ever, Contraception.none, UTI.ever, Trich.ever, 
          GenHerpes.ever, Pregnancy.cat, 
          smoking.current, Symptom.pain, Tampon.use.1mth, CST, condoms.48h, 
          Feminine.products, Feminine.products.48hrs)

##subset 1B2 dataset
a2 <- a %>%
  select (Participants, Nugent.score, Age, BMI, 
          BV..number.of.episodes.2.months., BV..number.of.episodes.year., 
          BV..number.of.episodes.lifetime., Yeast..2months., 
          Yeast..year., Yeast..lifetime., Antimicrobial.Use..y.1..n.0., 
          X.Non..Prescription..y.1..n.0.,
          Vaginal.intercourse.in.past.48.hours..y.1..n.0., 
          Freq.oral.sex.cat, Freq.anal.sex.cat, Freq.sex.toy.use.cat, 
          Abnormal.discharge.2wks, Abnormal.odor.2wks, 
          Irritation.Discomfort.2wks, Other.Symptoms.2wks, 
          Abnormal.discharge.48hrs, Abnormal.odor.48hrs, 
          Irritation.Discomfort.48hrs, Other.Symptoms.48hrs, 
          Genwarts.ever, Number.partners.in.past.year.cat, 
          contraception.H, contraception.B.M, contraception.C.IUD, 
          Substance.Use, study_arm, Ethnicity.cat, Tampon.Use.cat, 
          Presence.Symptoms.2wks, Presence.Symptoms.48hrs, 
          Chlamydia.ever, Contraception.none, UTI.ever, Trich.ever, 
          GenHerpes.ever, Pregnancy.cat, 
          smoking.current, Symptom.pain, Tampon.use.1mth, CST, condoms.48h, 
          Feminine.products, Feminine.products.48hrs)

#merge a and b together
c <-join(a2, b3, type="full")

#write this to file
#write.csv(c, "1A_1B2_compare.csv")
#####################################################################################
#Compare 1A and 1B2
#load data set
total <- read.csv(file = "1A_1B2_compare.csv")

#convert to factor
total$Antimicrobial.Use..y.1..n.0. <- factor(total$Antimicrobial.Use..y.1..n.0.)
total$X.Non..Prescription..y.1..n.0. <- factor(total$X.Non..Prescription..y.1..n.0.)
total$Vaginal.intercourse.in.past.48.hours..y.1..n.0. <- factor(total$Vaginal.intercourse.in.past.48.hours..y.1..n.0.)
total$Freq.oral.sex.cat <- factor(total$Freq.oral.sex.cat)
total$Freq.anal.sex.cat <- factor(total$Freq.anal.sex.cat)
total$Freq.sex.toy.use.cat <- factor(total$Freq.sex.toy.use.cat)
total$Abnormal.discharge.2wks <- factor(total$Abnormal.discharge.2wks)
total$Abnormal.odor.2wks <- factor(total$Abnormal.odor.2wks)
total$Irritation.Discomfort.2wks <- factor(total$Irritation.Discomfort.2wks)
total$Other.Symptoms.2wks <- factor(total$Other.Symptoms.2wks)
total$Abnormal.discharge.48hrs <- factor(total$Abnormal.discharge.48hrs)
total$Abnormal.odor.48hrs <- factor(total$Abnormal.odor.48hrs)
total$Irritation.Discomfort.48hrs <- factor(total$Irritation.Discomfort.48hrs)
total$Other.Symptoms.48hrs <- factor(total$Other.Symptoms.48hrs)
total$Genwarts.ever <- factor(total$Genwarts.ever)
total$Number.partners.in.past.year.cat <- factor(total$Number.partners.in.past.year.cat)
total$contraception.H <- factor(total$contraception.H)
total$contraception.B.M <- factor(total$contraception.B.M)
total$contraception.C.IUD <- factor(total$contraception.C.IUD)
total$Substance.Use <- factor(total$Substance.Use)
total$study_arm <- factor(total$study_arm)
total$Ethnicity.cat <- factor(total$Ethnicity.cat)
total$Tampon.Use.cat <- factor(total$Tampon.Use.cat)
total$Presence.Symptoms.2wks <- factor(total$Presence.Symptoms.2wks)
total$Presence.Symptoms.48hrs <- factor(total$Presence.Symptoms.48hrs)
total$Chlamydia.ever <- factor(total$Chlamydia.ever)
total$Contraception.none <- factor(total$Contraception.none)
total$UTI.ever <- factor(total$UTI.ever)
total$Trich.ever <- factor(total$Trich.ever)
total$GenHerpes.ever <- factor(total$GenHerpes.ever)
total$Pregnancy.cat <- factor(total$Pregnancy.cat)
total$smoking.current <- factor(total$smoking.current)
total$Symptom.pain <- factor(total$Symptom.pain)
total$Tampon.use.1mth <- factor(total$Tampon.use.1mth)
total$CST <- factor(total$CST)
total$condoms.48h <- factor(total$condoms.48h)
total$Feminine.products <- factor(total$Feminine.products)
total$Feminine.products.48hrs <- factor(total$Feminine.products.48hrs)

#continuous variables in glm
#Nugent score
mylogit <- glm(formula = study_arm ~ Nugent.score, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
confint(mylogit)

#Age
mylogit <- glm(formula = study_arm ~ Age, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
confint(mylogit)

#BMI
mylogit <- glm(formula = study_arm ~ BMI, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
confint(mylogit)

#BV..number.of.episodes.2.months.
mylogit <- glm(formula = study_arm ~ BV..number.of.episodes.2.months., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
confint(mylogit)

#BV..number.of.episodes.year.
mylogit <- glm(formula = study_arm ~ BV..number.of.episodes.year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
confint(mylogit)

#BV..number.of.episodes.lifetime.
mylogit <- glm(formula = study_arm ~ BV..number.of.episodes.lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
confint(mylogit)

#Yeast..2months.
mylogit <- glm(formula = study_arm ~ Yeast..2months., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
confint(mylogit)

#Yeast..year.
mylogit <- glm(formula = study_arm ~ Yeast..year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
confint(mylogit)

#Yeast..lifetime.
mylogit <- glm(formula = study_arm ~ Yeast..lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
confint(mylogit)


#cat variables in fishers
#CST
a <- xtabs(~study_arm + CST , data = total)
fisher.test(a)

#Ethnicity.cat
#Antimicrobial.Use..y.1..n.0.
#X.Non..Prescription..y.1..n.0.

#Symptoms
#Presence.Symptoms.2wks
#Abnormal.discharge.2wks
#Abnormal.odor.2wks
#Irritation.Discomfort.2wks
#Other.Symptoms.2wks
#Presence.Symptoms.48hrs
#Abnormal.discharge.48hrs
#Abnormal.odor.48hrs
#Irritation.Discomfort.48hrs
#Other.Symptoms.48hrs
#Symptom.pain

#Sexual Activity
#Vaginal.intercourse.in.past.48.hours..y.1..n.0.
#Freq.oral.sex.cat
#Freq.anal.sex.cat
#Freq.sex.toy.use.cat
#Number.partners.in.past.year.cat
#Contraception.none

#Genital Infection History
#Genwarts.ever
#Chlamydia.ever
#UTI.ever
#Trich.ever
#GenHerpes.ever

#Contraception
#contraception.H
#contraception.B.M
#contraception.C.IUD
#condoms.48h
#Pregnancy.cat

#Product use
#Feminine.products
#Feminine.products.48hrs
#Tampon.Use.cat
#Tampon.use.1mth

#Substance use
#smoking.current
#Substance.Use

