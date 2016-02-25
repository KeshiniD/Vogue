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
total <- read.csv(file.path("1A_grouped.csv"))
#remove random columns
total$X  <-  NULL
total$X.1  <-  NULL
total$X.2  <-  NULL

#need to be factors if wish to treat like categories
total$Age.cat <- factor(total$Age.cat)
total$BMI.under.cat <- factor(total$BMI.under.cat)
total$BMI.over.cat <- factor(total$BMI.over.cat)
total$Ethnicity.cat <- factor(total$Ethnicity.cat)
total$antimicrodrug <- factor(total$antimicrodrug)
total$rxdrug <- factor(total$rxdrug)
total$Tampon.Use.cat <- factor(total$Tampon.Use.cat)
total$vaginalintercourse48hr <- factor(total$vaginalintercourse48hr)
total$Presence.Symptoms.2wks <- factor(total$Presence.Symptoms.2wks)
total$abnormaldischarge2wk <- factor(total$abnormaldischarge2wk)
total$abnormalodor2wk <- factor(total$abnormalodor2wk)
total$irritationdiscomfort2wk <- factor(total$irritationdiscomfort2wk)
total$vaginalsymptomother2wk <- factor(total$vaginalsymptomother2wk)
total$Presence.Symptoms.48hrs <- factor(total$Presence.Symptoms.48hrs)
total$abnormaldischarge48 <- factor(total$abnormaldischarge48)
total$abnormalodor48 <- factor(total$abnormalodor48)
total$irritationdiscomfort48 <- factor(total$irritationdiscomfort48)
total$vaginalsymptomother48 <- factor(total$vaginalsymptomother48)
total$Contraception.H <- factor(total$Contraception.H)
total$Contraception.B.M <- factor(total$Contraception.B.M)
total$Contraception.IUD <- factor(total$Contraception.IUD)
total$condoms.48h <- factor(total$condoms.48h)
total$CSTI <- factor(total$CSTI)
total$CSTII <- factor(total$CSTII)
total$CSTIII <- factor(total$CSTIII)
total$CSTIVA <- factor(total$CSTIVA)
total$CSTIVC <- factor(total$CSTIVC)
total$CSTIVD <- factor(total$CSTIVD)
total$CSTV <- factor(total$CSTV)
total$oralsxfrequency.cat <- factor(total$oralsxfrequency.cat)
total$analsxfrequency.cat <- factor(total$analsxfrequency.cat)
total$sextoyfrequency.cat <- factor(total$sextoyfrequency.cat)
total$Chlamydia.ever <- factor(total$Chlamydia.ever)
total$Condyloma.ever <- factor(total$Condyloma.ever)
total$sexpartner1yr.cat <- factor(total$sexpartner1yr.cat)
total$UTI.ever <- factor(total$UTI.ever)
total$Trich.ever <- factor(total$Trich.ever)
total$GenHerpes.ever <- factor(total$GenHerpes.ever)
total$Pregnancy.cat <- factor(total$Pregnancy.cat)
total$Feminine.products <- factor(total$Feminine.products)
total$Feminine.products.48hrs <- factor(total$Feminine.products.48hrs)
total$substance_use_yn <- factor(total$substance_use_yn)
total$smoking.current <- factor(total$smoking.current)
total$Symptom.pain <- factor(total$Symptom.pain)
total$Contraception.none <- factor(total$Contraception.none)
total$Tampon.use.1mth <- factor(total$Tampon.use.1mth)
total$Gonorrhea.ever <- factor(total$Gonorrhea.ever)
total$Syphillis.ever <- factor(total$Syphillis.ever)


#2X2 tables-Fishers
#pvalue <0.05 reject null and there is assocation
#pvalue >0.05 do not reject null and there is no association
#CSTI
#Demographics
a <- xtabs(~CSTI + Age.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTI + BMI.under.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTI + BMI.over.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Ethnicity.cat , data = total)
fisher.test(a)

#genital infections
a <- xtabs(~CSTI + UTI.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Chlamydia.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Condyloma.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Trich.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTI + GenHerpes.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Gonorrhea.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Syphillis.ever , data = total)
fisher.test(a)
#meds
a <- xtabs(~CSTI + antimicrodrug , data = total)
fisher.test(a)

a <- xtabs(~CSTI + rxdrug , data = total)
fisher.test(a)

#symptoms
a <- xtabs(~CSTI + Presence.Symptoms.2wks , data = total)
fisher.test(a)

a <- xtabs(~CSTI + abnormaldischarge2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTI + abnormalodor2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTI + irritationdiscomfort2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTI + vaginalsymptomother2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Presence.Symptoms.48hrs , data = total)
fisher.test(a)

a <- xtabs(~CSTI + abnormaldischarge48 , data = total)
fisher.test(a)

a <- xtabs(~CSTI + abnormalodor48 , data = total)
fisher.test(a)

a <- xtabs(~CSTI + irritationdiscomfort48 , data = total)
fisher.test(a)

a <- xtabs(~CSTI + vaginalsymptomother48 , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Symptom.pain , data = total)
fisher.test(a)

#Sexual Activity
a <- xtabs(~CSTI + vaginalintercourse48hr , data = total)
fisher.test(a)

a <- xtabs(~CSTI + oralsxfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTI + analsxfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTI + sextoyfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTI + sexpartner1yr.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Contraception.H , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Contraception.B.M , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Contraception.IUD , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Contraception.none , data = total)
fisher.test(a)

a <- xtabs(~CSTI + condoms.48h , data = total)
fisher.test(a)

#Pregnancy
a <- xtabs(~CSTI + Pregnancy.cat , data = total)
fisher.test(a)

#Product use
a <- xtabs(~CSTI + Feminine.products , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Feminine.products.48hrs , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Tampon.Use.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTI + Tampon.use.1mth , data = total)
fisher.test(a)

#substance use
a <- xtabs(~CSTI + substance_use_yn , data = total)
fisher.test(a)

a <- xtabs(~CSTI + smoking.current , data = total)
fisher.test(a)

#######
#t-test (since 2 groups, if it was 2+ then ANOVA) for BV and Yeast episodes

#CSTI
#BV episodes
t.test(bv_infecttotal_2mo~CSTI, data = total)
t.test(bv_infecttotal_1yr~CSTI, data = total)
t.test(bv_life~CSTI, data = total)

#Yeast episodes
t.test(yeast_infecttotal_2mo~CSTI, data = total)
t.test(yeast_infecttotal_1yr~CSTI, data = total)
t.test(yeast_life~CSTI, data = total)

###########################################################
#CSTIII
#Demographics
a <- xtabs(~CSTIII + Age.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + BMI.under.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + BMI.over.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Ethnicity.cat , data = total)
fisher.test(a)

#genital infections
a <- xtabs(~CSTIII + UTI.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Chlamydia.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Condyloma.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Trich.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + GenHerpes.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Gonorrhea.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Syphillis.ever , data = total)
fisher.test(a)
#meds
a <- xtabs(~CSTIII + antimicrodrug , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + rxdrug , data = total)
fisher.test(a)

#symptoms
a <- xtabs(~CSTIII + Presence.Symptoms.2wks , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + abnormaldischarge2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + abnormalodor2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + irritationdiscomfort2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + vaginalsymptomother2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Presence.Symptoms.48hrs , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + abnormaldischarge48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + abnormalodor48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + irritationdiscomfort48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + vaginalsymptomother48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Symptom.pain , data = total)
fisher.test(a)

#Sexual Activity
a <- xtabs(~CSTIII + vaginalintercourse48hr , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + oralsxfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + analsxfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + sextoyfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + sexpartner1yr.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Contraception.H , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Contraception.B.M , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Contraception.IUD , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Contraception.none , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + condoms.48h , data = total)
fisher.test(a)

#Pregnancy
a <- xtabs(~CSTIII + Pregnancy.cat , data = total)
fisher.test(a)

#Product use
a <- xtabs(~CSTIII + Feminine.products , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Feminine.products.48hrs , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Tampon.Use.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Tampon.use.1mth , data = total)
fisher.test(a)

#substance use
a <- xtabs(~CSTIII + substance_use_yn , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + smoking.current , data = total)
fisher.test(a)

#######
#t-test (since 2 groups, if it was 2+ then ANOVA) for BV and Yeast episodes

#CSTIII
#BV episodes
t.test(bv_infecttotal_2mo~CSTIII, data = total)
t.test(bv_infecttotal_1yr~CSTIII, data = total)
t.test(bv_life~CSTIII, data = total)

#Yeast episodes
t.test(yeast_infecttotal_2mo~CSTIII, data = total)
t.test(yeast_infecttotal_1yr~CSTIII, data = total)
t.test(yeast_life~CSTIII, data = total)

##############################################################################
#CSTIVA
#Demographics
a <- xtabs(~CSTIII + Age.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + BMI.under.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + BMI.over.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Ethnicity.cat , data = total)
fisher.test(a)

#genital infections
a <- xtabs(~CSTIII + UTI.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Chlamydia.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Condyloma.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Trich.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + GenHerpes.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Gonorrhea.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Syphillis.ever , data = total)
fisher.test(a)
#meds
a <- xtabs(~CSTIII + antimicrodrug , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + rxdrug , data = total)
fisher.test(a)

#symptoms
a <- xtabs(~CSTIII + Presence.Symptoms.2wks , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + abnormaldischarge2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + abnormalodor2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + irritationdiscomfort2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + vaginalsymptomother2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Presence.Symptoms.48hrs , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + abnormaldischarge48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + abnormalodor48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + irritationdiscomfort48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + vaginalsymptomother48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Symptom.pain , data = total)
fisher.test(a)

#Sexual Activity
a <- xtabs(~CSTIII + vaginalintercourse48hr , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + oralsxfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + analsxfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + sextoyfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + sexpartner1yr.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Contraception.H , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Contraception.B.M , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Contraception.IUD , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Contraception.none , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + condoms.48h , data = total)
fisher.test(a)

#Pregnancy
a <- xtabs(~CSTIII + Pregnancy.cat , data = total)
fisher.test(a)

#Product use
a <- xtabs(~CSTIII + Feminine.products , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Feminine.products.48hrs , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Tampon.Use.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + Tampon.use.1mth , data = total)
fisher.test(a)

#substance use
a <- xtabs(~CSTIII + substance_use_yn , data = total)
fisher.test(a)

a <- xtabs(~CSTIII + smoking.current , data = total)
fisher.test(a)

#######
#t-test (since 2 groups, if it was 2+ then ANOVA) for BV and Yeast episodes

#CSTIII
#BV episodes
t.test(bv_infecttotal_2mo~CSTIII, data = total)
t.test(bv_infecttotal_1yr~CSTIII, data = total)
t.test(bv_life~CSTIII, data = total)

#Yeast episodes
t.test(yeast_infecttotal_2mo~CSTIII, data = total)
t.test(yeast_infecttotal_1yr~CSTIII, data = total)
t.test(yeast_life~CSTIII, data = total)