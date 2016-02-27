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