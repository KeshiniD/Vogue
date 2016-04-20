#load packages
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)

#load dataset
data <- read.csv(file="VOGUE_1A_1B_1B2.csv")
virome <- read.csv(file="Virome_Participants.csv")
virome$X <- NULL

#just want 1A and 1B2
newvirome <- data.frame(virome[-c(22:46), ]  )

#select virome participants
data <- read.csv(file="VOGUE_1A_1B_1B2.csv")
#newvirome
nums <- substring(newvirome$VogueViromeParticipants, 1)
ids <- paste0("", nums)
data2 <- data[which(data$study_id %in% ids), ]

#merge newvirome participants (has CSTs) with data2
#rename vogueviromeparticpants to study_id
newvirome <- dplyr::rename(newvirome, study_id = VogueViromeParticipants) #same column name

total<-join(data2, newvirome, type="full") #merge

#write to file
#write.csv(total, "virome_metadata_full.csv")

################################################################################
#put metadata into groups like 1B, then merge 1A, 1B2 data with 1B data 
#1B already grouped

#clean up variables, group
total <- read.csv(file="virome_metadata_full.csv")

#Grouping variables
#Age
total$Age.cat[total$age < 20] <- "1" 
total$Age.cat[total$age >= 20 & total$age <=29.9] <- "2"
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

#Ethnicity with First Nations as cat
total$Ethnicity2.cat[total$ethwhite___1>=1] <- '1'
total$Ethnicity2.cat[total$ethblack___1>=1] <- '3'
total$Ethnicity2.cat[total$ethhisp___1>=1] <- '3'
total$Ethnicity2.cat[total$ethasian___1>=1] <- '3'
total$Ethnicity2.cat[total$ethsasian___1>=1] <- '3'
total$Ethnicity2.cat[total$ethfirstnation___1>=1] <- '2'
total$Ethnicity2.cat[total$ethother___1>=1] <- '3'

#convert Ethnicity.cat from character into factor
total$Ethnicity2.cat <- factor(total$Ethnicity2.cat)
############

#leave BV epsiodes alone
############
#Genital Infections
#collapse BV category, BV ever
total$BV.ever <- ifelse(total$bv_life > 0, 
                        c("1"), c("0")) 
#convert BV.ever from character into factor
total$BV.ever <- factor(total$BV.ever) 

#collapse Yeast category, Yeast ever
total$Yeast.ever <- ifelse(total$yeast_life > 0, 
                           c("1"), c("0")) 
#convert Yeast.ever from character into factor
total$Yeast.ever <- factor(total$Yeast.ever) 

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

############################################
#convert integers into factors
#Antimicrobial use in the past 3 months
# yes-1, no-2 --> yes-1, no-0
total$antimicrodrug <- ifelse(total$antimicrodrug > 1, 
                              c("0"), c("1"))
total$antimicrodrug <- factor(total$antimicrodrug)

#convert integers into factors
#(non)Prescription use in the past 2 months
# yes-1, no-2 --> yes-1, no-0
total$rxdrug <- ifelse(total$rxdrug > 1, 
                       c("0"), c("1"))
total$rxdrug <- factor(total$rxdrug)
########################

#Symptoms
#create for 48hrs and 2 weeks
#add new column for presence of symptoms in 2 weeks
total$Presence.Symptoms.2wks <- ifelse(total$abnormaldischarge2wk > 0 |
                                         total$abnormalodor2wk > 0 |
                                         total$irritationdiscomfort2wk > 0 |
                                         total$vaginalsymptomother2wk > 0, 
                                       c("1"), c("0")) 

#convert numeric into factor
# yes-1, no-0
total$Presence.Symptoms.2wks <- factor(total$Presence.Symptoms.2wks)

#add new column for presence of symptoms in 48hrs
total$Presence.Symptoms.48hrs <- ifelse(total$abnormaldischarge48 > 0 |
                                          total$abnormalodor48 > 0 |
                                          total$irritationdiscomfort48 > 0 |
                                          total$vaginalsymptomother48 > 0, 
                                        c("1"), c("0")) 

#convert numeric into factor
# yes-1, no-0
total$Presence.Symptoms.48hrs <- factor(total$Presence.Symptoms.48hrs)
###################
#sxpain
#convert sx.pain from character into factor
total$Symptom.pain <- ifelse(total$vagintercoursediscomfort > 1, 
                             c("0"), c("1"))
total$Symptom.pain <- factor(total$Symptom.pain)

#######################

#convert integers into factors
#vaginal intercourse in the past 48 hours
# yes-1, no-0
total$vaginalintercourse48hr <- ifelse(total$vaginalintercourse48hr > 1, 
                                       c("0"), c("1"))
total$vaginalintercourse48hr <- factor(total$vaginalintercourse48hr)
##########################

#Frequency of Oral Sex
#create categories (change to never-ever cats)
total$oralsxfrequency.cat[total$oralsxfrequency < 2] <- '0' #yes-1, no-0
total$oralsxfrequency.cat[total$oralsxfrequency > 1] <- '1' #yes-1, no-0

#convert Freq.oral.sex.cat from character into factor
total$oralsxfrequency.cat <- as.factor(total$oralsxfrequency.cat)

#Frequency of Anal Sex
#create categories (change to never-ever cats)
total$analsxfrequency.cat[total$analsxfrequency < 2] <- '0' #yes-1, no-0
total$analsxfrequency.cat[total$analsxfrequency > 1] <- '1' #yes-1, no-0

#convert Freq.anal.sex.cat from character into factor
total$analsxfrequency.cat <- as.factor(total$analsxfrequency.cat)

#Frequency of Sex Toy Use
#create categories (change to never-ever cats)
total$sextoyfrequency.cat[total$sextoyfrequency < 2] <- '0' #yes-1, no-0
total$sextoyfrequency.cat[total$sextoyfrequency > 1] <- '1' #yes-1, no-0

#convert Freq.sex.toy.cat from character into factor
total$sextoyfrequency.cat <- as.factor(total$sextoyfrequency.cat)

##########################
#Sexual Partners
#male-1, female-2, both-3, virgin-4
#convert sexpartners from character into factor
total$sexpartner <- factor(total$sexpartner)
###########################################

#change to never-ever cats. 
##Cat. for number of sexual partners in the last year
total$sexpartner1yr.cat[total$sexpartner1yr <= 0] <- "0" # 0 partners
total$sexpartner1yr.cat[total$sexpartner1yr >= 1] <- "1"#1 or more partners

#convert Number.partners.in.past.year.cat from character into factor
total$sexpartner1yr.cat <- factor(total$sexpartner1yr.cat)

##Cat. for number of sexual partners in the last 2 months
total$sexpartner2mo.cat[total$sexpartner2mo <= 0] <- "0" # 0 partners
total$sexpartner2mo.cat[total$sexpartner2mo >= 1] <- "1"#1 or more partners

#convert Number.partners.in.past.year.cat from character into factor
total$sexpartner2mo.cat <- factor(total$sexpartner2mo.cat)

################################
#Contraception
#condense categories (none-S, B, H and IUD): y-n categories (1-0)
total$Contraception.H <- ifelse(total$contramethhormonal___1 >=1, 
                                c("1"), c("0")) 

total$Contraception.B.M <- ifelse(total$contramethbarriermc___1 >=1, 
                                  c("1"), c("0")) 

total$Contraception.IUD <- ifelse(total$contramethbarrieriud___1 >=1, 
                                  c("1"), c("0")) 

total$Contraception.none <- ifelse(total$contramethnone___1>=1 | 
                                     total$contramethsurgstersubj___1>=1 |
                                     total$contramethsurgsterpart___1>=1, 
                                   c("1"), c("0")) 

#convert Contraception cats from character into factor
total$Contraception.H <- factor(total$Contraception.H)
total$Contraception.B.M <- factor(total$Contraception.B.M)
total$Contraception.IUD <- factor(total$Contraception.IUD)
total$Contraception.none <- factor(total$Contraception.none)
total$contramethnotactive___1 <- factor(total$contramethnotactive___1)

###########################################
#Use of condoms in last 48 hours
total$condoms.48h <- ifelse(total$vaginalintercourse48hr == '1' &
                              total$Contraception.B.M == '1', 
                            c("1"), c("0")) 
#convert 48h.uses.condoms from character into factor
total$condoms.48h <- factor(total$condoms.48h)
############################################

#Condense pregnancy into null vs multi
# multi->1, null-0
total$pregnancyhistoryg
total$Pregnancy.cat[total$pregnancyhistoryg <= 0] <- "0" # null
#total$Pregnancy.cat[total$pregnancyhistoryg <= 1 & total$pregnancyhistoryg >= 1] <- "1"#1 preg
total$Pregnancy.cat[total$pregnancyhistoryg > 1] <- "1"#multi

#convert Pregnancy.cat from character into factor
total$Pregnancy.cat <- factor(total$Pregnancy.cat) 
summary(total$Pregnancy.cat)

###############################################

#douche products and feminine wipes
#combine two variables into feminine product use
total$Feminine.products <- ifelse(total$doucheproducts == '1' |
                                    total$deodorantproducts == '1', 
                                  c("1"), c("0")) 
total$Feminine.products <- factor(total$Feminine.products) 
summary(total$Feminine.products)

#used feminine products in past 48 hours
#combine two variables into feminine product use
total$Feminine.products.48hrs <- ifelse(total$douche48hrs == '1' |
                                          total$deodorant48hrs == '1', 
                                        c("1"), c("0")) 
total$Feminine.products.48hrs <- factor(total$Feminine.products.48hrs) 
summary(total$Feminine.products.48hrs)

##########################################################

#Frequency of Tampon Use
#create categories (never-ever cats)
total$Tampon.Use.cat <- ifelse(total$tamponusage == '1', 
                               c("0"), c("1")) 

#convert Tampon.Use.cat from character into factor
total$Tampon.Use.cat <- factor(total$Tampon.Use.cat)

#######################################################
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
                                  total$tamponusage >= 3, 
                                c("1"), c("0")) 

#convert Tampon.use.1mth from character into factor
total$Tampon.use.1mth <- factor(total$Tampon.use.1mth)
summary(total$Tampon.use.1mth)

#####################################

#smoking (yes-1, no-0)
#condense to currently smoking or not
total$smoking.current <- ifelse(total$tobaccouse == '2', 
                                c("1"), c("0")) 
#convert Substance.Use from character into factor
total$smoking.current <- factor(total$smoking.current)
summary(total$smoking.current)

#######################################
#drug use
#manually created
total[,"druguse"]  <- c(2,2,1,2,0,0,2,1,0,2,0,0,0,0,0,
                        2,0,1,0,0,2,1,1,1,1,0,1,1,0)

#convert numeric into factor
# current-2, past-1, no-0
total$druguse <- factor(total$druguse)

#substance use (drug and alcohol) 
#combine: current (yes-1,no-0)(all alcohol drinkers are current) 
total$substanceuse <- ifelse(total$druguse > 1 & 
                               total$alcoholcurrent >= 3, 
                             c("1"), c("0")) 

#convert substanceuse from character into factor
total$substanceuse <- factor(total$substanceuse)
summary(total$substanceuse)

###################################################################

#select categories want to analyse
total2 <- total %>%
  select(study_id, CST, Age.cat, BMI.under.cat, BMI.over.cat, Ethnicity.cat, Ethnicity2.cat, 
         bv_life, bv_infecttotal_1yr, bv_infecttotal_2mo, BV.ever, Yeast.ever, 
         UTI.ever, Trich.ever, Condyloma.ever, GenHerpes.ever, 
         Chlamydia.ever, Gonorrhea.ever, Syphillis.ever, Presence.Symptoms.2wks, 
         Presence.Symptoms.48hrs, Symptom.pain, oralsxfrequency.cat, 
         analsxfrequency.cat, sextoyfrequency.cat, sexpartner1yr.cat, 
         sexpartner2mo.cat, Contraception.H, Contraception.B.M, Contraception.IUD, 
         Contraception.none, condoms.48h, Pregnancy.cat, Feminine.products, 
         Feminine.products.48hrs, Tampon.Use.cat, days.since.LMP, 
         Tampon.use.1mth, smoking.current, druguse, substanceuse, 
         nugent_score_result, sexpartner, contramethnotactive___1, 
         abnormaldischarge2wk, abnormaldischarge48, 
         abnormalodor2wk, abnormalodor48, irritationdiscomfort2wk, 
         irritationdiscomfort48, vaginalsymptomother2wk, vaginalsymptomother48, 
         rxdrug, antimicrodrug)

#write to file
#write.csv(total2, "virome_metadata_grouped.csv")

#####
#back to start; and include 1B
#dataset I have is only hpv types, not all virome 1B