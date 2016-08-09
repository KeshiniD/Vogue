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
a <- xtabs(~CSTIVA + Age.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + BMI.under.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + BMI.over.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Ethnicity.cat , data = total)
fisher.test(a)

#genital infections
a <- xtabs(~CSTIVA + UTI.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Chlamydia.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Condyloma.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Trich.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + GenHerpes.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Gonorrhea.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Syphillis.ever , data = total)
fisher.test(a)
#meds
a <- xtabs(~CSTIVA + antimicrodrug , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + rxdrug , data = total)
fisher.test(a)

#symptoms
a <- xtabs(~CSTIVA + Presence.Symptoms.2wks , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + abnormaldischarge2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + abnormalodor2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + irritationdiscomfort2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + vaginalsymptomother2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Presence.Symptoms.48hrs , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + abnormaldischarge48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + abnormalodor48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + irritationdiscomfort48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + vaginalsymptomother48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Symptom.pain , data = total)
fisher.test(a)

#Sexual Activity
a <- xtabs(~CSTIVA + vaginalintercourse48hr , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + oralsxfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + analsxfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + sextoyfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + sexpartner1yr.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Contraception.H , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Contraception.B.M , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Contraception.IUD , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Contraception.none , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + condoms.48h , data = total)
fisher.test(a)

#Pregnancy
a <- xtabs(~CSTIVA + Pregnancy.cat , data = total)
fisher.test(a)

#Product use
a <- xtabs(~CSTIVA + Feminine.products , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Feminine.products.48hrs , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Tampon.Use.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + Tampon.use.1mth , data = total)
fisher.test(a)

#substance use
a <- xtabs(~CSTIVA + substance_use_yn , data = total)
fisher.test(a)

a <- xtabs(~CSTIVA + smoking.current , data = total)
fisher.test(a)

#######
#t-test (since 2 groups, if it was 2+ then ANOVA) for BV and Yeast episodes

#CSTIVA
#BV episodes
t.test(bv_infecttotal_2mo~CSTIVA, data = total)
t.test(bv_infecttotal_1yr~CSTIVA, data = total)
t.test(bv_life~CSTIVA, data = total)

#Yeast episodes
t.test(yeast_infecttotal_2mo~CSTIVA, data = total)
t.test(yeast_infecttotal_1yr~CSTIVA, data = total)
t.test(yeast_life~CSTIVA, data = total)
##############################################################################
#CSTIVC
#Demographics
a <- xtabs(~CSTIVC + Age.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + BMI.under.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + BMI.over.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Ethnicity.cat , data = total)
fisher.test(a)

#genital infections
a <- xtabs(~CSTIVC + UTI.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Chlamydia.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Condyloma.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Trich.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + GenHerpes.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Gonorrhea.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Syphillis.ever , data = total)
fisher.test(a)

#meds
a <- xtabs(~CSTIVC + antimicrodrug , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + rxdrug , data = total)
fisher.test(a)

#symptoms
a <- xtabs(~CSTIVC + Presence.Symptoms.2wks , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + abnormaldischarge2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + abnormalodor2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + irritationdiscomfort2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + vaginalsymptomother2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Presence.Symptoms.48hrs , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + abnormaldischarge48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + abnormalodor48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + irritationdiscomfort48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + vaginalsymptomother48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Symptom.pain , data = total)
fisher.test(a)

#Sexual Activity
a <- xtabs(~CSTIVC + vaginalintercourse48hr , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + oralsxfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + analsxfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + sextoyfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + sexpartner1yr.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Contraception.H , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Contraception.B.M , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Contraception.IUD , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Contraception.none , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + condoms.48h , data = total)
fisher.test(a)

#Pregnancy
a <- xtabs(~CSTIVC + Pregnancy.cat , data = total)
fisher.test(a)

#Product use
a <- xtabs(~CSTIVC + Feminine.products , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Feminine.products.48hrs , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Tampon.Use.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + Tampon.use.1mth , data = total)
fisher.test(a)

#substance use
a <- xtabs(~CSTIVC + substance_use_yn , data = total)
fisher.test(a)

a <- xtabs(~CSTIVC + smoking.current , data = total)
fisher.test(a)

#######
#t-test (since 2 groups, if it was 2+ then ANOVA) for BV and Yeast episodes

#CSTIVC
#BV episodes
t.test(bv_infecttotal_2mo~CSTIVC, data = total)
t.test(bv_infecttotal_1yr~CSTIVC, data = total)
t.test(bv_life~CSTIVC, data = total)

#Yeast episodes
t.test(yeast_infecttotal_2mo~CSTIVC, data = total)
t.test(yeast_infecttotal_1yr~CSTIVC, data = total)
t.test(yeast_life~CSTIVC, data = total)
###############################################################################
#CSTIVD
#Demographics
a <- xtabs(~CSTIVD + Age.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + BMI.under.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + BMI.over.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Ethnicity.cat , data = total)
fisher.test(a)

#genital infections
a <- xtabs(~CSTIVD + UTI.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Chlamydia.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Condyloma.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Trich.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + GenHerpes.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Gonorrhea.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Syphillis.ever , data = total)
fisher.test(a)
#meds
a <- xtabs(~CSTIVD + antimicrodrug , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + rxdrug , data = total)
fisher.test(a)

#symptoms
a <- xtabs(~CSTIVD + Presence.Symptoms.2wks , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + abnormaldischarge2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + abnormalodor2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + irritationdiscomfort2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + vaginalsymptomother2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Presence.Symptoms.48hrs , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + abnormaldischarge48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + abnormalodor48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + irritationdiscomfort48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + vaginalsymptomother48 , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Symptom.pain , data = total)
fisher.test(a)

#Sexual Activity
a <- xtabs(~CSTIVD + vaginalintercourse48hr , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + oralsxfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + analsxfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + sextoyfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + sexpartner1yr.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Contraception.H , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Contraception.B.M , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Contraception.IUD , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Contraception.none , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + condoms.48h , data = total)
fisher.test(a)

#Pregnancy
a <- xtabs(~CSTIVD + Pregnancy.cat , data = total)
fisher.test(a)

#Product use
a <- xtabs(~CSTIVD + Feminine.products , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Feminine.products.48hrs , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Tampon.Use.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + Tampon.use.1mth , data = total)
fisher.test(a)

#substance use
a <- xtabs(~CSTIVD + substance_use_yn , data = total)
fisher.test(a)

a <- xtabs(~CSTIVD + smoking.current , data = total)
fisher.test(a)

#######
#t-test (since 2 groups, if it was 2+ then ANOVA) for BV and Yeast episodes

#CSTIVD
#BV episodes
t.test(bv_infecttotal_2mo~CSTIVD, data = total)
t.test(bv_infecttotal_1yr~CSTIVD, data = total)
t.test(bv_life~CSTIVD, data = total)

#Yeast episodes
t.test(yeast_infecttotal_2mo~CSTIVD, data = total)
t.test(yeast_infecttotal_1yr~CSTIVD, data = total)
t.test(yeast_life~CSTIVD, data = total)
################################################################################
#CSTV
#Demographics
a <- xtabs(~CSTV + Age.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTV + BMI.under.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTV + BMI.over.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Ethnicity.cat , data = total)
fisher.test(a)

#genital infections
a <- xtabs(~CSTV + UTI.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Chlamydia.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Condyloma.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Trich.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTV + GenHerpes.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Gonorrhea.ever , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Syphillis.ever , data = total)
fisher.test(a)

#meds
a <- xtabs(~CSTV + antimicrodrug , data = total)
fisher.test(a)

a <- xtabs(~CSTV + rxdrug , data = total)
fisher.test(a)

#symptoms
a <- xtabs(~CSTV + Presence.Symptoms.2wks , data = total)
fisher.test(a)

a <- xtabs(~CSTV + abnormaldischarge2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTV + abnormalodor2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTV + irritationdiscomfort2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTV + vaginalsymptomother2wk , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Presence.Symptoms.48hrs , data = total)
fisher.test(a)

a <- xtabs(~CSTV + abnormaldischarge48 , data = total)
fisher.test(a)

a <- xtabs(~CSTV + abnormalodor48 , data = total)
fisher.test(a)

a <- xtabs(~CSTV + irritationdiscomfort48 , data = total)
fisher.test(a)

a <- xtabs(~CSTV + vaginalsymptomother48 , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Symptom.pain , data = total)
fisher.test(a)

#Sexual Activity
a <- xtabs(~CSTV + vaginalintercourse48hr , data = total)
fisher.test(a)

a <- xtabs(~CSTV + oralsxfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTV + analsxfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTV + sextoyfrequency.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTV + sexpartner1yr.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Contraception.H , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Contraception.B.M , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Contraception.IUD , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Contraception.none , data = total)
fisher.test(a)

a <- xtabs(~CSTV + condoms.48h , data = total)
fisher.test(a)

#Pregnancy
a <- xtabs(~CSTV + Pregnancy.cat , data = total)
fisher.test(a)

#Product use
a <- xtabs(~CSTV + Feminine.products , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Feminine.products.48hrs , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Tampon.Use.cat , data = total)
fisher.test(a)

a <- xtabs(~CSTV + Tampon.use.1mth , data = total)
fisher.test(a)

#substance use
a <- xtabs(~CSTV + substance_use_yn , data = total)
fisher.test(a)

a <- xtabs(~CSTV + smoking.current , data = total)
fisher.test(a)

#######
#t-test (since 2 groups, if it was 2+ then ANOVA) for BV and Yeast episodes

#CSTV
#BV episodes
t.test(bv_infecttotal_2mo~CSTV, data = total)
t.test(bv_infecttotal_1yr~CSTV, data = total)
t.test(bv_life~CSTV, data = total)

#Yeast episodes
t.test(yeast_infecttotal_2mo~CSTV, data = total)
t.test(yeast_infecttotal_1yr~CSTV, data = total)
t.test(yeast_life~CSTV, data = total)

###################################################################################
#adjust p-values; Benjamini Hochburg
pvals <- read.csv(file = "clipboard") #copied from excel
pvals

a <- p.adjust(pvals$P.Value, method = 'hochberg', n = 104)
View(a)

####
#four variables significant; none in same cat so no multivariate
mylogit <- glm(formula = CSTIVA ~ bv_life, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

mylogit <- glm(formula = CSTI ~ nugent_score, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

mylogit <- glm(formula = CSTIVC ~ nugent_score_result, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

mylogit <- glm(formula = CSTIVD ~ nugent_score_result, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#######################################################################################
#UniPlot Odds
CST<- read.csv(file="1A_Uni.csv")

#PLOT
CST$colour <- ifelse(CST$Estimate < 0, "negative","positive")
CST$hjust <- ifelse(CST$Estimate > 0, 1.3, -0.3)
ggplot(CST,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)") + 
  ggtitle("CST Univariate Logistic Regression") + 
  theme(plot.title = element_text(size=22), 
        axis.title = element_text(size=16,face="bold"), 
        axis.text = element_text(size=14, face="bold"))
