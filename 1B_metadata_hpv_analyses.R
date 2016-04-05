#load packages
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)
library(vcd)

#load dataset
total <- read.csv(file="1B_grouped.csv")

#convert appropriate variables into factor, integers
total$t06 <- factor(total$t06)
total$t11 <- factor(total$t11)
total$t16 <- factor(total$t16)
total$t18 <- factor(total$t18)
total$t26 <- factor(total$t26)
total$t31 <- factor(total$t31)
total$t33 <- factor(total$t33)
total$t34 <- factor(total$t34)
total$t35 <- factor(total$t35)
total$t39 <- factor(total$t39)
total$t40 <- factor(total$t40)
total$t42 <- factor(total$t42)
total$t44 <- factor(total$t44)
total$t45 <- factor(total$t45)
total$t51 <- factor(total$t51)
total$t52 <- factor(total$t52)
total$t53 <- factor(total$t53)
total$t54 <- factor(total$t54)
total$t56 <- factor(total$t56)
total$t58 <- factor(total$t58)
total$t59 <- factor(total$t59)
total$t61 <- factor(total$t61)
total$t62 <- factor(total$t62)
total$t66 <- factor(total$t66)
total$t67 <- factor(total$t67)
total$t68 <- factor(total$t68)
total$t69 <- factor(total$t69)
total$t70 <- factor(total$t70)
total$t71 <- factor(total$t71)
total$t72 <- factor(total$t72)
total$t73 <- factor(total$t73)
total$t81 <- factor(total$t81)
total$t82 <- factor(total$t82)
total$t83 <- factor(total$t83)
total$t84 <- factor(total$t84)
total$t89 <- factor(total$t89)
total$CST <- factor(total$CST)
total$Age.cat <- factor(total$Age.cat)
total$BMI.under.cat <- factor(total$BMI.under.cat)
total$BMI.over.cat <- factor(total$BMI.over.cat)
total$Ethnicity.cat <- factor(total$Ethnicity.cat)
total$BV.ever <- factor(total$BV.ever)
total$Yeast.ever <- factor(total$Yeast.ever)
total$UTI.ever <- factor(total$UTI.ever)
total$Ethnicity2.cat <- factor(total$Ethnicity2.cat)
total$Trich.ever <- factor(total$Trich.ever)
total$Condyloma.ever <- factor(total$Condyloma.ever)
total$GenHerpes.ever <- factor(total$GenHerpes.ever)
total$Chlamydia.ever <- factor(total$Chlamydia.ever)
total$Gonorrhea.ever <- factor(total$Gonorrhea.ever)
total$Syphillis.ever <- factor(total$Syphillis.ever)
total$Presence.Symptoms.2wks <- factor(total$Presence.Symptoms.2wks)
total$Presence.Symptoms.48hrs <- factor(total$Presence.Symptoms.48hrs)
total$Symptom.pain <- factor(total$Symptom.pain)
total$oralsxfrequency.cat <- factor(total$oralsxfrequency.cat)
total$analsxfrequency.cat <- factor(total$analsxfrequency.cat)
total$sextoyfrequency.cat <- factor(total$sextoyfrequency.cat)
total$sexpartner1yr.cat <- factor(total$sexpartner1yr.cat)
total$Contraception.H <- factor(total$Contraception.H)
total$Contraception.B.M <- factor(total$Contraception.B.M)
total$Contraception.IUD <- factor(total$Contraception.IUD)
total$Contraception.none <- factor(total$Contraception.none)
total$condoms.48h <- factor(total$condoms.48h)
total$Pregnancy.cat <- factor(total$Pregnancy.cat)
total$Feminine.products <- factor(total$Feminine.products)
total$Feminine.products.48hrs <- factor(total$Feminine.products.48hrs)
total$Tampon.Use.cat <- factor(total$Tampon.Use.cat)
total$Tampon.use.1mth <- factor(total$Tampon.use.1mth)
total$smoking.current <- factor(total$smoking.current)
total$druguse <- factor(total$druguse)
total$substanceuse <- factor(total$substanceuse)
total$Is.the.patient.antiretroviral.naive. <- factor(total$Is.the.patient.antiretroviral.naive.)
total$HIV.Clade...Result <- factor(total$HIV.Clade...Result)
total$Likely.mode.of.HIV.acquisition <- factor(total$Likely.mode.of.HIV.acquisition)
total$HCV.Antibody...Result <- factor(total$HCV.Antibody...Result)
total$HCV.PCR...Result <- factor(total$HCV.PCR...Result)
total$HBV.sAb...Result <- factor(total$HBV.sAb...Result)
total$HBV.sAg...Result <- factor(total$HBV.sAg...Result)
total$HBV.cAb...Result <- factor(total$HBV.cAb...Result)
total$nugent_score_result <- factor(total$nugent_score_result)
total$sexpartner <- factor(total$sexpartner)
total$contramethnotactive___1 <- factor(total$contramethnotactive___1)
total$abnormaldischarge2wk <- factor(total$abnormaldischarge2wk)
total$abnormaldischarge48 <- factor(total$abnormaldischarge48)
total$abnormalodor2wk <- factor(total$abnormalodor2wk)
total$abnormalodor48 <- factor(total$abnormalodor48)
total$irritationdiscomfort2wk <- factor(total$irritationdiscomfort2wk)
total$irritationdiscomfort48 <- factor(total$irritationdiscomfort48)
total$vaginalsymptomother2wk <- factor(total$vaginalsymptomother2wk)
total$vaginalsymptomother48 <- factor(total$vaginalsymptomother48)
total$antimicrodrug <- factor(total$antimicrodrug)
total$rxdrug <- factor(total$rxdrug)

#t06, t11, 34, 39, 44, 69, 82
#have only 1 factor level (0 participants have these types)

#nugent (consistent -1, intermediate -2, inconsistent -3)

##2X2 tables-Fishers
#pvalue <0.05 reject null and there is assocation
#pvalue >0.05 do not reject null and there is no association

#t16
#Demographics
a <- xtabs(~t16 + Age.cat , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + BMI.under.cat , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + BMI.over.cat , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Ethnicity.cat , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Ethnicity2.cat , data = total)
fisher.test(a)
assocstats(a)

#Meds
a <- xtabs(~t16 + antimicrodrug , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + rxdrug , data = total)
fisher.test(a)
assocstats(a)

#Genital Infections
mylogit <- glm(formula = t16 ~ bv_infecttotal_2mo, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

mylogit <- glm(formula = t16 ~ bv_infecttotal_1yr, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

mylogit <- glm(formula = t16 ~ bv_life, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

a <- xtabs(~t16 + BV.ever , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Yeast.ever , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + UTI.ever , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Trich.ever , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Condyloma.ever , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + GenHerpes.ever , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Chlamydia.ever , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Gonorrhea.ever , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Syphillis.ever , data = total)
fisher.test(a)
assocstats(a)

#Symptoms
a <- xtabs(~t16 + Presence.Symptoms.2wks , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Presence.Symptoms.48hrs , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + abnormaldischarge2wk , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + abnormaldischarge48 , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + abnormalodor2wk , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + abnormalodor48 , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + irritationdiscomfort2wk , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + irritationdiscomfort48 , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + vaginalsymptomother2wk , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + vaginalsymptomother48 , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Symptom.pain , data = total)
fisher.test(a)
assocstats(a)


#Contraception
a <- xtabs(~t16 + Contraception.H , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Contraception.B.M , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Contraception.IUD , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Contraception.none , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + contramethnotactive___1 , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + condoms.48h , data = total)
fisher.test(a)
assocstats(a)

#Pregnancy
a <- xtabs(~t16 + Pregnancy.cat , data = total)
fisher.test(a)
assocstats(a)

#Sexual Activty
a <- xtabs(~t16 + oralsxfrequency.cat , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + analsxfrequency.ca , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + sextoyfrequency.cat , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + sexpartner1yr.ca , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + sexpartner , data = total)
fisher.test(a)
assocstats(a)

#Product Use
a <- xtabs(~t16 + Feminine.products , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Feminine.products.48hrs , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Tampon.Use.cat , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Tampon.use.1mth , data = total)
fisher.test(a)
assocstats(a)

#Substance Use
a <- xtabs(~t16 + smoking.current , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + druguse , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + substanceuse , data = total)
fisher.test(a)
assocstats(a)

#HIV data
a <- xtabs(~t16 + Is.the.patient.antiretroviral.naive. , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + HIV.Clade...Result , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + Likely.mode.of.HIV.acquisition , data = total)
fisher.test(a)
assocstats(a)

mylogit <- glm(formula = t16 ~ Med.Duration, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

mylogit <- glm(formula = t16 ~ Duration.of.HIV.Infection., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

mylogit <- glm(formula = t16 ~ CD4.Nadir., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

mylogit <- glm(formula = t16 ~ Highest.VL.Ever.., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

mylogit <- glm(formula = t16 ~ CD4., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

mylogit <- glm(formula = t16 ~ VL..copies.mL.., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

a <- xtabs(~t16 + HCV.Antibody...Result , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + HCV.PCR...Result , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + HBV.sAb...Result , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + HBV.sAg...Result , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~t16 + HBV.cAb...Result , data = total)
fisher.test(a)
assocstats(a)

#HPV data
mylogit <- glm(formula = t16 ~ Number.of.Different.HPV.Types, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#CST
a <- xtabs(~t16 + CST , data = total)
fisher.test(a)
assocstats(a)

#Nugent
a <- xtabs(~t16 + nugent_score_result , data = total)
fisher.test(a)
assocstats(a)

