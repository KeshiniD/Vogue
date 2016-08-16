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
#write.csv(total, "1A_full_grouped.csv")
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
#remember to rerun factor code above
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
######################################################################################
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

##################################################################
#Aug-2-16
#contin variables should go in t.test

#Nugent score
t.test(Nugent.score ~ study_arm, data=total)
#Age
t.test(Age ~ study_arm, data=total)
#BMI
t.test(BMI ~ study_arm, data=total)
#BV..number.of.episodes.2.months.
t.test(BV..number.of.episodes.2.months. ~ study_arm, data=total)
#BV..number.of.episodes.year.
t.test(BV..number.of.episodes.year. ~ study_arm, data=total)
#BV..number.of.episodes.lifetime.
t.test(BV..number.of.episodes.lifetime. ~ study_arm, data=total)
#Yeast..2months.
t.test(Yeast..2months. ~ study_arm, data=total)
#Yeast..year.
t.test(Yeast..year. ~ study_arm, data=total)
#Yeast..lifetime.
t.test(Yeast..lifetime. ~ study_arm, data=total)

###########################################
#cat variables in fishers
#CST
a <- xtabs(~study_arm + CST , data = total)
fisher.test(a)

#BV ever and Yeast ever.cat
total$BV.ever <- ifelse(total$BV..number.of.episodes.lifetime. > 0, 
                        c("1"), c("0"))
total$BV.ever <- factor(total$BV.ever)

total$Yeast.ever <- ifelse(total$Yeast..lifetime. > 0, 
                           c("1"), c("0"))
total$Yeast.ever <- factor(total$Yeast.ever)

a <- xtabs(~study_arm + BV.ever, data = total)
fisher.test(a)

a <- xtabs(~study_arm + Yeast.ever, data = total)
fisher.test(a)

#Ethnicity.cat
a <- xtabs(~study_arm + Ethnicity.cat, data = total)
fisher.test(a)

#Antimicrobial.Use..y.1..n.0.
a <- xtabs(~study_arm + Antimicrobial.Use..y.1..n.0., data = total)
fisher.test(a)

#X.Non..Prescription..y.1..n.0.
a <- xtabs(~study_arm + X.Non..Prescription..y.1..n.0., data = total)
fisher.test(a)

#Symptoms
#Presence.Symptoms.2wks
a <- xtabs(~study_arm + Presence.Symptoms.2wks, data = total)
fisher.test(a)

#Abnormal.discharge.2wks
a <- xtabs(~study_arm + Abnormal.discharge.2wks, data = total)
fisher.test(a)

#Abnormal.odor.2wks
a <- xtabs(~study_arm + Abnormal.odor.2wks, data = total)
fisher.test(a)

#Irritation.Discomfort.2wks
a <- xtabs(~study_arm + Irritation.Discomfort.2wks, data = total)
fisher.test(a)

#Other.Symptoms.2wks
a <- xtabs(~study_arm + Other.Symptoms.2wks, data = total)
fisher.test(a)

#Presence.Symptoms.48hrs
a <- xtabs(~study_arm + Presence.Symptoms.48hrs, data = total)
fisher.test(a)

#Abnormal.discharge.48hrs
a <- xtabs(~study_arm + Abnormal.discharge.48hrs, data = total)
fisher.test(a)

#Abnormal.odor.48hrs
a <- xtabs(~study_arm + Abnormal.odor.48hrs, data = total)
fisher.test(a)

#Irritation.Discomfort.48hrs
a <- xtabs(~study_arm + Irritation.Discomfort.48hrs, data = total)
fisher.test(a)

#Other.Symptoms.48hrs
a <- xtabs(~study_arm + Other.Symptoms.48hrs, data = total)
fisher.test(a)

#Symptom.pain
a <- xtabs(~study_arm + Symptom.pain, data = total)
fisher.test(a)

#Sexual Activity
#Vaginal.intercourse.in.past.48.hours..y.1..n.0.
a <- xtabs(~study_arm + Vaginal.intercourse.in.past.48.hours..y.1..n.0., data = total)
fisher.test(a)

#Freq.oral.sex.cat
a <- xtabs(~study_arm + Freq.oral.sex.cat, data = total)
fisher.test(a)

#Freq.anal.sex.cat
a <- xtabs(~study_arm + Freq.anal.sex.cat, data = total)
fisher.test(a)

#Freq.sex.toy.use.cat
a <- xtabs(~study_arm + Freq.sex.toy.use.cat, data = total)
fisher.test(a)

#Number.partners.in.past.year.cat
a <- xtabs(~study_arm + Number.partners.in.past.year.cat, data = total)
fisher.test(a)

#Genital Infection History
#Genwarts.ever
a <- xtabs(~study_arm + Genwarts.ever, data = total)
a <- matrix(c(289,22,21,4), 2, 2,)
fisher.test(a)

#Chlamydia.ever
a <- xtabs(~study_arm + Chlamydia.ever, data = total)
a <- matrix(c(238,21,24,5), 2, 2,)
fisher.test(a)

#UTI.ever
a <- xtabs(~study_arm + UTI.ever, data = total)
a <- matrix(c(103,10,160,15), 2, 2,)
fisher.test(a)

#Trich.ever
a <- xtabs(~study_arm + Trich.ever, data = total)
a <- matrix(c(259,25,3,0), 2, 2,)
fisher.test(a)

#GenHerpes.ever
a <- xtabs(~study_arm + GenHerpes.ever, data = total)
a <- matrix(c(248,22,15,4), 2, 2,)
fisher.test(a)

#Gonorrhea.ever and Syphillis.ever
summary(factor(b$Gonorrhea.ever))
a <- matrix(c(259,0,5,0), 2, 2,)
fisher.test(a)

summary(factor(b$Syphillis.ever))
a <- matrix(c(262,0,1,0), 2, 2,)
fisher.test(a)

#Contraception
#contraception.H
a <- xtabs(~study_arm + contraception.H, data = total)
fisher.test(a)

#contraception.B.M
a <- xtabs(~study_arm + contraception.B.M, data = total)
fisher.test(a)

#contraception.C.IUD
a <- xtabs(~study_arm + contraception.C.IUD, data = total)
fisher.test(a)

#Contraception.none
a <- xtabs(~study_arm + Contraception.none, data = total)
fisher.test(a)

#condoms.48h
a <- xtabs(~study_arm + condoms.48h, data = total)
fisher.test(a)

#Pregnancy.cat
a <- xtabs(~study_arm + Pregnancy.cat, data = total)
fisher.test(a)

#Product use
#Feminine.products
a <- xtabs(~study_arm + Feminine.products, data = total)
fisher.test(a)

#Feminine.products.48hrs
a <- xtabs(~study_arm + Feminine.products.48hrs, data = total)
fisher.test(a)

#Tampon.Use.cat
a <- xtabs(~study_arm + Tampon.Use.cat, data = total)
fisher.test(a)

#Tampon.use.1mth
a <- xtabs(~study_arm + Tampon.use.1mth, data = total)
fisher.test(a)

#Substance use
#smoking.current
a <- xtabs(~study_arm + smoking.current, data = total)
fisher.test(a)

#Substance.Use
a <- xtabs(~study_arm + Substance.Use, data = total)
fisher.test(a)

#Martial Status
a <- matrix(c(172,11,133,10, 1, 5), 2, 3,)

#Education
a <- matrix(c(3,1,7,2, 87, 7,145,14,60,2,8,0), 2, 6,)

#Menstrual Period
a <- matrix(c(225,23,48,1,2,0,31,2), 2, 4,)

#Sexual Partners
a <- matrix(c(278,24,9,1,13,0,10,0), 2, 4,)

################################################################################
#1B2 features in 1A
total <- read.csv(file="1A_full_grouped.csv")

#used above code to fix symptom variables

#make new CST.cats
#add new categories
#CSTI (yes-1, no-0)
total$CSTI <- ifelse(total$CST == 'I', 
                     c("1"), c("0")) 
#convert CSTI from character into factor
total$CSTI <- factor(total$CSTI)

#CSTIII (yes-1, no-0)
total$CSTIII <- ifelse(total$CST == 'III', 
                       c("1"), c("0")) 
#convert CSTIII from character into factor
total$CSTIII <- factor(total$CSTIII)

#CSTIVA (yes-1, no-0)
total$CSTIVA <- ifelse(total$CST == 'IVA', 
                       c("1"), c("0")) 
#convert CSTIVA from character into factor
total$CSTIVA <- factor(total$CSTIVA)

#CSTIVC (yes-1, no-0)
total$CSTIVC <- ifelse(total$CST == 'IVC', 
                       c("1"), c("0")) 
#convert CSTIVC from character into factor
total$CSTIVC <- factor(total$CSTIVC)

#CSTIVD (yes-1, no-0)
total$CSTIVD <- ifelse(total$CST == 'IVD', 
                       c("1"), c("0")) 
#convert CSTI from character into factor
total$CSTIVD <- factor(total$CSTIVD)

#CSTV (yes-1, no-0)
total$CSTV <- ifelse(total$CST == 'V', 
                     c("1"), c("0")) 
#convert CSTV from character into factor
total$CSTV <- factor(total$CSTV)

#significant variables in 1B2 now in 1A
total$abnormaldischarge48 <- factor(total$abnormaldischarge48)
total$abnormalodor2wk <- factor(total$abnormalodor2wk)
total$abnormalodor48 <- factor(total$abnormalodor48)
total$Contraception.none <- factor(total$Contraception.none)
total$Contraception.H <- factor(total$Contraception.H)
total$Contraception.B.M <- factor(total$Contraception.B.M)
total$sexpartner1yr.cat <- factor(total$sexpartner1yr.cat)

#CSTIII
a <- xtabs(~CSTIII + abnormaldischarge48 , data = total)
fisher.test(a)

#CSTIVA
a <- xtabs(~CSTIVA + abnormalodor2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + abnormalodor48 , data = total)
fisher.test(a)

#CSTIVC
a <- xtabs(~CSTIVC + Contraception.H , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Contraception.B.M , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Contraception.none , data = total)
fisher.test(a)

#episodes of BV
mylogit <- glm(formula = CSTIVC ~ bv_infecttotal_2mo, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ bv_infecttotal_1yr, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#CSTIVD
a <- xtabs(~CSTIVD + sexpartner1yr.cat , data = total)
fisher.test(a)

#significant variables in 1A, look at in 1B2
total <- read.csv(file="1B2metabac_condensedv2.csv")

#factor
total$CST <- factor(total$CST)
total$Ethnicity.cat <- factor(total$Ethnicity.cat)

#Ethnicity
a <- xtabs(~CST + Ethnicity.cat , data = total)
fisher.test(a)

#Nugent.score
mylogit <- glm(formula = CST ~ Nugent.score, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#SD
mylogit <- glm(formula = CST ~ Shannon.s.Diversity, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#################################################################################
#Aug-2-16
#compare 1A and 1B2, diversity statistics

#load diversity statistics for 1A
chao1A <- read.csv("1A_individual_chao1.csv")
chao1A$X <- NULL
richness1A <- read.csv("1A_individual_richness.csv")
pielou1A <- read.csv("1A_individual_Pielou.csv")
SD1A <- read.csv("1A_individual_diversity.csv")
good1A <- read.csv("1A_individual_Good.csv")
good1A$X <- NULL

#load diversity statisitcs for 1B2
all1B2 <- read.csv("1B2_individual_all5_diversity.csv")
all1B2$X <- NULL

#rename headers in 1A to match 
chao1A <- dplyr::rename(chao1A, Participants = var, Chao1 = chao) 
richness1A <- dplyr::rename(richness1A, Participants = X, Species.Richness = rarefy) 
pielou1A <- dplyr::rename(pielou1A, Pielou.s.Eveness = J) 
SD1A <- dplyr::rename(SD1A, Shannon.s.Diversity = ShannonsDiversity) 
good1A <- dplyr::rename(good1A, Good.s.Coverage = a) 

#join all 1A together, and merge with 1B2
diversity <- cbind(chao1A, richness1A, pielou1A, SD1A, good1A)
#remove duplicate columns
diversity2 <- diversity[ -c(3, 5, 7, 9) ]

#add study_arm 
all1B2[,"study_arm"]  <- c("1B2")
diversity2[,"study_arm"]  <- c("1A")

#merge 1B2
diversity3 <- join(diversity2, all1B2, type="full")

#write to file
#write.csv(diversity3, "1A_1B2_diversitystats.csv")

#call data back
diversity <- read.csv("1A_1B2_diversitystats.csv")

#independent:study_arm, dependent:contin diversity stats
t.test(Shannon.s.Diversity ~study_arm, data=diversity)
t.test(Pielou.s.Eveness ~study_arm, data=diversity)
t.test(Chao1 ~study_arm, data=diversity)
t.test(Species.Richness ~study_arm, data=diversity)
t.test(Good.s.Coverage ~study_arm, data=diversity)

###
#aug-16-16
#redid bv 2 months as a category; and year and lifetime for comparasion
#grouping BV.2months
total2$BV.2mths.cat <- ifelse(total2$BV..number.of.episodes.2.months. > 0, 
                              c("1"), c("0")) 
#convert UTI.ever from character into factor
total2$BV.2mths.cat <- factor(total2$BV.2mths.cat) 

result <- xtabs(~ BV.2mths.cat + study_arm, data = total)
fisher.test(result)

result <- xtabs(~ BV.year.cat + study_arm, data = total)
fisher.test(result)

result <- xtabs(~ BV.lifetime.cat + study_arm, data = total)
fisher.test(result)
