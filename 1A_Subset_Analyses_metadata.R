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

#participants randomly selected as seen in "1A_randomized_selection.csv"

### DEAN ADDED THIS
#does what the multiple lines below does so deleted them
dean <- read.csv("../Vogue/1A_randomized_selection.csv")
nums <- substring(dean$Study.ID, 12)
ids <- paste0("Vogue 1A 01-", nums)
total <- data[which(data$study_id %in% ids), ]
###

############################
#Grouping variables
#Age
total$Age.cat[total$age < 20] <- "1" 
total$Age.cat[total$age >= 20 & total$age <=29] <- "2"
total$Age.cat[total$age >= 30] <- "3" 
#total$Age.cat[total$Age >= 30 & Age <=39] <- "3" #combo 30-39 wiht 40+
#total$Age.cat[total$Age >= 40] <- "3" 

#convert Age.cat from character into factor
total$Age.cat <- as.factor(total$Age.cat)

###############
#BMI 
#Two categories; compare underweight to normal and overweight/obese to normal
#Underweight and Normal weight
total$BMI.under.cat[total$bmi < 18.5] <- "1" #underweight
total$BMI.under.cat[total$bmi >= 18.5 & total$bmi <=24.9] <- "2" #normal weight
total$BMI.under.cat[total$bmi >= 25] <- "" #overweight/obese

#convert BMI.cat from character into factor
total$BMI.under.cat <- as.factor(total$BMI.under.cat) 

#OVerweight/Obese and Normal weight
total$BMI.over.cat[total$bmi < 18.5] <- "" #underweight
total$BMI.over.cat[total$bmi >= 18.5 & total$bmi <=24.9] <- "2" #normal weight
total$BMI.over.cat[total$bmi >= 25] <- "3" #overweight/obese

#convert BMI.cat from character into factor
total$BMI.over.cat <- factor(total$BMI.over.cat) 

###########
#Ethnicity 
#combine Other, Aboriginal, and Asian
total$Ethnicity.cat[total$ethwhite___1>=1] <- '1'
total$Ethnicity.cat[total$ethblack___1>=1] <- '2'
total$Ethnicity.cat[total$ethhisp___1>=1] <- '2'
total$Ethnicity.cat[total$ethasian___1>=1] <- '2'
total$Ethnicity.cat[total$ethsasian___1>=1] <- '2'
total$Ethnicity.cat[total$ethfirstnation___1>=1] <- '2'
total$Ethnicity.cat[total$ethother___1>=1] <- '2'

#convert Ethnicity.cat from character into factor
total$Ethnicity.cat <- factor(total$Ethnicity.cat)

############
#leave BV and Yeast episodes alone
############
#Genital Infections
#collapse UTI category, UTI ever
total$UTI.ever <- ifelse(total$uti_life > 0, 
                         c("1"), c("0")) 
#convert UTI.ever from character into factor
total$UTI.ever <- factor(total$UTI.ever) 

#collapse Trich category, Trich ever
total$Trich.ever <- ifelse(total$trich_life > 0, 
                           c("1"), c("0")) 
#convert Trich.ever from character into factor
total$Trich.ever <- factor(total$Trich.ever) 

#collapse Genital Warts category, GenWarts ever
total$Condyloma.ever <- ifelse(total$condy_life > 0, 
                              c("1"), c("0")) 
#convert Genwarts.ever from character into factor
total$Condyloma.ever <- factor(total$Condyloma.ever) 

#collapse Genital Herpes category, GenHerpes ever
total$GenHerpes.ever <- ifelse(total$herpes_infecttotal_life > 0, 
                               c("1"), c("0")) 
#convert GenHerpes.ever from character into factor
total$GenHerpes.ever <- factor(total$GenHerpes.ever) 

#collapse Chlamydia category, Chlamydia ever
total$Chlamydia.ever <- ifelse(total$chlam_life > 0, 
                               c("1"), c("0")) 
#convert Chlamydia.ever from character into factor
total$Chlamydia.ever <- factor(total$Chlamydia.ever) 

#collapse Gonorrhea category, Gonorrhea ever
total$Gonorrhea.ever <- ifelse(total$gonor_life > 0, 
                               c("1"), c("0")) 
#convert Gonorrhea.ever from character into factor
total$Gonorrhea.ever <- factor(total$Gonorrhea.ever) 

#collapse Syphillis category, Syphillis ever
total$Syphillis.ever <- ifelse(total$syph_life > 0, 
                               c("1"), c("0")) 
#convert Syphillis.ever from character into factor
total$Syphillis.ever <- factor(total$Syphillis.ever) 

#####################
#convert integers into factors
#Antimicrobial use in the past 3 months
# yes-1, no-2
total$antimicrodrug <- factor(total$antimicrodrug)

#convert integers into factors
#(non)Prescription use in the past 2 months
# yes-1, no-2
total$rxdrug <- factor(total$rxdrug)
########################

#manually created
#Taken probiotics within 2 months
total[,"probiotics.2.months"]  <- c(0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,
                                    0,0,0,1,1,0,0,0,1)
#convert numeric into factor
# yes-1, no-2
total$probiotics.2.months <- factor(total$probiotics.2.months)
#######################

#Symptoms
#create for 48hrs and 2 weeks
#add new column for presence of symptoms in 2 weeks
total$Presence.Symptoms.2wks <- ifelse(total$abnormaldischarge2wk > 0 |
                                         total$abnormalodor2wk > 0 |
                                         total$irritationdiscomfort2wk > 0 |
                                         total$vaginalsymptomother2wk > 0, 
                               c("1"), c("2")) 

#convert numeric into factor
# yes-1, no-2
total$Presence.Symptoms.2wks <- factor(total$Presence.Symptoms.2wks)

#add new column for presence of symptoms in 48hrs
total$Presence.Symptoms.48hrs <- ifelse(total$abnormaldischarge48 > 0 |
                                         total$abnormalodor48 > 0 |
                                         total$irritationdiscomfort48 > 0 |
                                         total$vaginalsymptomother48 > 0, 
                                       c("1"), c("2")) 

#convert numeric into factor
# yes-1, no-2
total$Presence.Symptoms.48hrs <- factor(total$Presence.Symptoms.48hrs)
###################

#sxpain
#convert sx.pain from character into factor
total$Symptom.pain <- factor(total$vagintercoursediscomfort)
summary(total$Symptom.pain)

########################
#convert integers into factors
#vaginal intercourse in the past 48 hours
# yes-1, no-2
total$vaginalintercourse48hr <- factor(total$vaginalintercourse48hr)
##########################

#Frequency of Oral Sex
#create categories (change to never-ever cats)
total$oralsxfrequency.cat[total$oralsxfrequency < 6] <- '1' #yes-1, no-2
total$oralsxfrequency.cat[total$oralsxfrequency > 5] <- '2' #yes-1, no-2

#convert Freq.oral.sex.cat from character into factor
total$oralsxfrequency.cat <- as.factor(total$oralsxfrequency.cat)

#Frequency of Anal Sex
#create categories (change to never-ever cats)
total$analsxfrequency.cat[total$analsxfrequency < 6] <- '1' #yes-1, no-2
total$analsxfrequency.cat[total$analsxfrequency > 5] <- '2' #yes-1, no-2

#convert Freq.anal.sex.cat from character into factor
total$analsxfrequency.cat <- as.factor(total$analsxfrequency.cat)

#Frequency of Sex Toy Use
#create categories (change to never-ever cats)
total$sextoyfrequency.cat[total$sextoyfrequency < 6] <- '1' #yes-1, no-2
total$sextoyfrequency.cat[total$sextoyfrequency > 5] <- '2' #yes-1, no-2

#convert Freq.sex.toy.cat from character into factor
total$sextoyfrequency.cat <- as.factor(total$sextoyfrequency.cat)

##########################

#Sexual Partners
#male-1, female-2, both-3, virgin-4
#needs to be character to reassign value
total$Sexual.Partners.cat[total$Sexual.Partners.cat=='Male'] <- '0'
total$Sexual.Partners.cat[total$Sexual.Partners.cat=='Female'] <- '1'

#convert Sexual.Partners.cat from character into factor
total$Sexual.Partners.cat <- factor(total$Sexual.Partners.cat)

###########################################
#change to never-ever cats. 
##Cat. for number of sexual partners in the last year
total$sexpartner1yr.cat[total$sexpartner1yr <= 0] <- "2" # 0 partners
total$sexpartner1yr.cat[total$sexpartner1yr >= 1] <- "1"#1 or more partners

#convert Number.partners.in.past.year.cat from character into factor
total$sexpartner1yr.cat <- factor(total$sexpartner1yr.cat)

################################
#Contraception
#condense categories (none-S, B, H and IUD): y-n categories (1-2)
total$Contraception.H <- ifelse(total$contramethhormonal___1 >=1, 
                                        c("1"), c("2")) 

total$Contraception.B.M <- ifelse(total$contramethbarriermc___1 >=1, 
                                c("1"), c("2")) 

total$Contraception.IUD <- ifelse(total$contramethbarrieriud___1 >=1, 
                                  c("1"), c("2")) 

total$Contraception.none <- ifelse(total$contramethnone___1>=1 | 
                                     total$contramethsurgstersubj___1>=1 |
                                     total$contramethsurgsterpart___1>=1, 
                                   c("1"), c("2")) 

#convert Contraception cats from character into factor
total$Contraception.H <- factor(total$Contraception.H)
total$Contraception.B.M <- factor(total$Contraception.B.M)
total$Contraception.IUD <- factor(total$Contraception.IUD)
total$Contraception.none <- factor(total$Contraception.none)

###########################################

#Use of condoms in last 48 hours
total$condoms.48h <- ifelse(total$vaginalintercourse48hr == '1' &
                              total$Contraception.B.M == '1', 
                            c("1"), c("2")) 
#convert 48h.uses.condoms from character into factor
total$condoms.48h <- factor(total$condoms.48h)
############################################
#Condense pregnancy into null vs multi
# multi-1, null-0
total$pregnancyhistoryg
total$Pregnancy.cat <- ifelse(total$pregnancyhistoryg > 1, 
                              c("1"), c("0")) 
#convert Pregnancy.cat from character into factor
total$Pregnancy.cat <- factor(total$Pregnancy.cat) 
summary(total$Pregnancy.cat)
###############################################

#douche products and feminine wipes
#combine two variables into feminine product use
total$Feminine.products <- ifelse(total$doucheproducts == '1' |
                                    total$deodorantproducts == '1', 
                                  c("1"), c("2")) 
total$Feminine.products <- factor(total$Feminine.products) 
summary(total$Feminine.products)

#used feminine products in past 48 hours
#combine two variables into feminine product use
total$Feminine.products.48hrs <- ifelse(total$douche48hrs == '1' |
                                          total$deodorant48hrs == '1', 
                                        c("1"), c("2")) 
total$Feminine.products.48hrs <- factor(total$Feminine.products.48hrs) 
summary(total$Feminine.products.48hrs)

##########################################################

#Frequency of Tampon Use
#create categories (never-ever cats)
total$Tampon.Use.cat <- ifelse(total$tamponusage == '4', 
                                        c("2"), c("1")) 

#convert Tampon.Use.cat from character into factor
total$Tampon.Use.cat <- factor(total$Tampon.Use.cat)

##########################################################
#Days since LMP
finish <- as.Date(total$last_menst_per, format="%m/%e/%Y")
start <- as.Date(total$date_study_entry, format="%m/%e/%Y")
date_diff<-as.data.frame(abs(finish-start))
total[,"days.since.LMP"] <- abs(finish-start)

#convert to integer
total$days.since.LMP <- as.integer(total$days.since.LMP)

#bundle LMP and tampon use: tampons used in past month
#menses within 30 days, do they use tampon every period (exclusively or partly)
total$Tampon.use.1mth <- ifelse(total$days.since.LMP <30 & 
                                  total$tamponusage<=2, 
                                c("1"), c("2")) 

#convert Tampon.use.1mth from character into factor
total$Tampon.use.1mth <- factor(total$Tampon.use.1mth)
summary(total$Tampon.use.1mth)
##############################################################

#Substance use
#change to never-ever cats
#emcompasses those who use past or currently
#convert Substance.Use from character into factor
total$substance_use_yn <- factor(total$substance_use_yn)

##############################################################

#smoking (yes-1, no-2)
#condense to currently smoking or not
total$smoking.current <- ifelse(total$tobaccouse == '2', 
                                c("1"), c("2")) 
#convert Substance.Use from character into factor
total$smoking.current <- factor(total$smoking.current)
summary(total$smoking.current)

#write to file
write.csv(total, "1A_grouped.csv")
