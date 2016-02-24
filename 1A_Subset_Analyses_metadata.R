##Diversity Indices
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


#grouping variables


############
#Age
#Age
attach(total)
total$Age.cat[Age < 20] <- "1" 
total$Age.cat[Age >= 20 & Age <=29] <- "2"
total$Age.cat[Age >= 30 & Age <=39] <- "3"
total$Age.cat[Age >= 40] <- "4" 
detach(total)

#convert Age.cat from character into factor
total$Age.cat <- factor(total$Age.cat)
#combine 30-39 with 40+
data$Age.cat[data$Age.cat > 3] <- "3" #reassinging 40+ to 30s cat
#convert Age.cat to factor
data$Age.cat <- as.factor(data$Age.cat)


###############
#BMI 
#Two categories; compare underweight to normal and overweight/obese to normal

#Underweight and Normal weight
data$BMI.under.cat[data$BMI < 18.5] <- "1" #underweight
data$BMI.under.cat[data$BMI >= 18.5 & data$BMI <=24.9] <- "2" #normal weight
data$BMI.under.cat[data$BMI >= 25] <- "" #overweight/obese

#convert BMI.cat from character into factor
data$BMI.under.cat <- factor(data$BMI.under.cat) 

#OVerweight/Obese and Normal weight
data$BMI.over.cat[data$BMI < 18.5] <- "" #underweight
data$BMI.over.cat[data$BMI >= 18.5 & data$BMI <=24.9] <- "2" #normal weight
data$BMI.over.cat[data$BMI >= 25] <- "3" #overweight/obese

#convert BMI.cat from character into factor
data$BMI.over.cat <- factor(data$BMI.over.cat) 

###########
#Ethnicity 
#combine Other, Aboriginal, and Asian
data$Ethnicity.cat[data$Ethnicity=='Caucasian'] <- '1'
data$Ethnicity.cat[data$Ethnicity=='Asian'] <- '2'
data$Ethnicity.cat[data$Ethnicity=='Aboriginal'] <- '2'
data$Ethnicity.cat[data$Ethnicity=='Other (Arab)'] <- '2'
data$Ethnicity.cat[data$Ethnicity=='Other (Haida/Scottish)'] <- '2'
#convert Ethnicity.cat from character into factor
data$Ethnicity.cat <- factor(data$Ethnicity.cat)

############
#leave BV and Yeast episodes alone
############
#Genital Infections
#collapse UTI category, UTI ever
total$UTI.ever <- ifelse(total$UTI..lifetime. > 0, 
                         c("1"), c("0")) 
#convert UTI.ever from character into factor
total$UTI.ever <- factor(total$UTI.ever) 
#remove other UTI categories
total$UTI..2.months. <- NULL
total$UTI..year. <- NULL
total$UTI..lifetime. <- NULL

#collapse Trich category, Trich ever
total$Trich.ever <- ifelse(total$Trich..lifetime.=='chronic', 
                           c("1"), c("0")) 
#convert Trich.ever from character into factor
total$Trich.ever <- factor(total$Trich.ever) 
#remove other TRich categories
total$Trich..2.months. <- NULL
total$Trich..year. <- NULL
total$Trich..lifetime. <- NULL

#collapse Genital Warts category, GenWarts ever
total$Genwarts.ever <- ifelse(total$Genital.Warts..lifetime. > 0, 
                              c("1"), c("0")) 
#convert Genwarts.ever from character into factor
total$Genwarts.ever <- factor(total$Genwarts.ever) 
#remove other UTI categories
total$Genital.Warts..2months. <- NULL
total$Genital.Warts..year. <- NULL
total$Genital.Warts..lifetime. <- NULL   

#collapse Genital Herpes category, GenHerpes ever
total$GenHerpes.ever <- ifelse(total$Genital.Herpes..lifetime. > 0, 
                               c("1"), c("0")) 
#convert GenHerpes.ever from character into factor
total$GenHerpes.ever <- factor(total$GenHerpes.ever) 
#remove other UTI categories
total$Genital.Herpes..2months. <- NULL
total$Genital.Herpes..year. <- NULL
total$Genital.Herpes..lifetime. <- NULL   

#collapse Genital Herpes category, GenHerpes ever
total$GenHerpes.ever <- ifelse(total$Genital.Herpes..lifetime. > 0, 
                               c("1"), c("0")) 
#convert GenHerpes.ever from character into factor
total$GenHerpes.ever <- factor(total$GenHerpes.ever) 
#remove other UTI categories
total$Genital.Herpes..2months. <- NULL
total$Genital.Herpes..year. <- NULL
total$Genital.Herpes..lifetime. <- NULL   

#already have chlamydia ever category
summary(total$Chlamydia.ever)
#remove other chlamydia categories
total$Chlamydia..2.months. <- NULL
total$Chlamydia..year. <- NULL
total$Chlamydia..lifetime. <- NULL   

total$Gonorrhea.ever #no levels
#remove and syphillis too
total$Gonorrhea <- NULL
total$Gonorrhea.ever <- NULL 
total$Syphillis <- NULL

#####################
#convert integers into factors
#Antimicrobial use in the past 3 months
# yes-1, no-0
total$Antimicrobial.Use..y.1..n.0. <- factor(total$Antimicrobial.Use..y.1..n.0.)

#convert integers into factors
#(non)Prescription use in the past 2 months
# yes-1, no-0
total$X.Non..Prescription..y.1..n.0. <- factor(total$X.Non..Prescription..y.1..n.0.)
########################

#manually created
#Taken probiotics within 2 months
total[,"probiotics.2.months"]  <- c(0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,
                                    0,0,0,1,1,0,0,0,1)
#convert numeric into factor
# yes-1, no-0
total$probiotics.2.months <- factor(total$probiotics.2.months)
#######################

#Symptoms
#create for 48hrs and 2 weeks
#add new column for presence of symptoms in 2 weeks
total[,"Presence.Symptoms.2wks"]  <- c(1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,1,0,1,
                                       1,1,1,1,1,1,1,1)
#convert numeric into factor
# yes-1, no-0
total$Presence.Symptoms.2wks <- factor(total$Presence.Symptoms.2wks)
###################

#create symptoms pain variable
#sxpain
summary(total$How.often.pain.experienced.during.vaginal.intercourse.percentage)
#condense to yes and no
total$Symptom.pain <- ifelse(total$How.often.pain.experienced.during.vaginal.intercourse.percentage > 0, 
                             c("1"), c("0")) 
#convert Substance.Use from character into factor
total$Symptom.pain <- factor(total$Symptom.pain)
summary(total$Symptom.pain)
########################
#convert integers into factors
#vaginal intercourse in the past 48 hours
# yes-1, no-0
total$Vaginal.intercourse.in.past.48.hours..y.1..n.0. <- factor(total$Vaginal.intercourse.in.past.48.hours..y.1..n.0.)
##########################

#Frequency of Oral Sex
#create categories (change to never-ever cats)
#data$Freq.oral.sex.cat[data$Freq.oral.sex.cat=='never'] <- '0'
data$Freq.oral.sex.cat[data$Freq.oral.sex.cat >0] <- '1' #need only run this

#convert Freq.oral.sex.cat from character into factor
data$Freq.oral.sex.cat <- factor(data$Freq.oral.sex.cat)

#same for anal and sex toy
##########################

#Sexual Partners
#needs to be character to reassign value
total$Sexual.Partners.cat <- mapply(as.character, total$Sexual.Partners)
#create categories
total$Sexual.Partners.cat[total$Sexual.Partners.cat=='Male'] <- '0'
total$Sexual.Partners.cat[total$Sexual.Partners.cat=='Female'] <- '1'

#convert Sexual.Partners.cat from character into factor
total$Sexual.Partners.cat <- factor(total$Sexual.Partners.cat)
#seemed to have removed this cat; nvm
#remove Sexual Partners (LATER)
summary(total$Sexual.Partners.cat)
#Remove non-cat variables
total$Sexual.Partners <- NULL

###########################################
#change to never-ever cats. 
##Cat. for number of sexual partners in the last year
data$Number.partners.in.past.year.cat[data$Number.partners.in.past.year <= 0] <- "0" # 0 partners
data$Number.partners.in.past.year.cat[data$Number.partners.in.past.year >= 1] <- "1"#1 or more partners

#convert Number.partners.in.past.year.cat from character into factor
data$Number.partners.in.past.year.cat <- factor(data$Number.partners.in.past.year.cat)

################################
#Contraception
summary(total$Contraception)
#condense categories (none-S, B, H and IUD): y-n categories
summary(total$contraception.H) #keep
summary(total$contraception.B.M) #keep
summary(total$contraception.C.IUD)#keep
#make new 'no contraception category
total$Contraception.none <- ifelse(total$Contraception=='None' | total$Contraception=='S', 
                                   c("1"), c("0")) 
#convert Contraception.none from character into factor
total$Contraception.none <- factor(total$Contraception.none)
summary(total$Contraception.none)

#remove the remaining categories
total$Contraception <- NULL
total$Contraception.cat <- NULL
total$contraception.S.S <- NULL
total$contraception.S.P <- NULL
total$contraception.B.F <- NULL
total$HContr.Progestin.pill <- NULL
total$HContr.Combination.pill <- NULL
total$HContr.nuvaring <- NULL
total$HContr.mirena <- NULL
total$HContr.depoprovera <- NULL
total$HContr.orthoevra <- NULL
total$contr_type <- NULL
###########################################

#Use of condoms in last 48 hours
total$condoms.48h <- ifelse(total$Vaginal.intercourse.in.past.48.hours..y.1..n.0. == '1' &
                              total$contraception.B.M == '1', 
                            c("1"), c("0")) 
#convert 48h.uses.condoms from character into factor
total$condoms.48h <- factor(total$condoms.48h)
############################################
#Condense pregnancy into null vs multi
# multi-1, null-0
total$Pregnancy.History..g.
total$Pregnancy.cat <- ifelse(total$Pregnancy.History..g. > 1, 
                              c("1"), c("0")) 
#convert Pregnancy.cat from character into factor
total$Pregnancy.cat <- factor(total$Pregnancy.cat) 
summary(total$Pregnancy.cat)
###############################################

#douche products and feminine wipes
summary(total$Use.of.douche.products..y.1..n.0.)
summary(total$Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0.)
#combine today variables into feminine product use
total$Feminine.products <- ifelse(total$Use.of.douche.products..y.1..n.0. == '1' |
                                    total$Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0. == '1', 
                                  c("1"), c("0")) 
total$Feminine.products <- factor(total$Feminine.products) 
summary(total$Feminine.products)

#remove douche and feminine variables
total$Use.of.douche.products..y.1..n.0.<- NULL
total$Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0.<- NULL

#used feminine products in past 48 hours
summary(total$Used.in.the.past.48.hours)#douche
summary(total$Used.in.past.48.hours)#feminine wipes
#combine today variables into feminine product use
total$Feminine.products.48hrs <- ifelse(total$Used.in.the.past.48.hours == '1' |
                                          total$Used.in.past.48.hours == '1', 
                                        c("1"), c("0")) 
total$Feminine.products.48hrs <- factor(total$Feminine.products.48hrs) 
summary(total$Feminine.products.48hrs)

#remove douche and feminine variables past 48hours
total$Used.in.the.past.48.hours<- NULL
total$Used.in.past.48.hours<- NULL
##########################################################

#Frequency of Tampon Use
#needs to be character to reassign value
data$Tampon.Use.cat <- mapply(as.character, data$Tampon.Use)
#create categories (never-ever cats)
data$Tampon.Use.cat[data$Tampon.Use.cat=='never'] <- '1'#never used
data$Tampon.Use.cat[data$Tampon.Use.cat=='sometimes but not for every period'] <- '2'#used ever
data$Tampon.Use.cat[data$Tampon.Use.cat=='every period, part of the time'] <- '2'
data$Tampon.Use.cat[data$Tampon.Use.cat=='every period, exclusively'] <- '2'

#convert Tampon.Use.cat from character into factor
data$Tampon.Use.cat <- factor(data$Tampon.Use.cat)

##########################################################
#Days since LMP
total[,"days.since.LMP"]  <- c(26,12,23,20,14,"",0,18,20,9,"",1,19,7,12,12,
                               "",99,14,8,9,19,20,9,18,13)
#convert character into integer
# yes-1, no-0
total$days.since.LMP <- as.integer(total$days.since.LMP)

#bundle LMP and tampon use: tampons used in past month
summary(total$Tampon.Use)
total$days.since.LMP
#menses within 30 days, do they use tampon every period (exclusively or partly)
total$Tampon.use.1mth <- ifelse(total$days.since.LMP <30 & 
                                  total$Tampon.Use =='every period, exclusively' | 
                                  total$Tampon.Use =='every period, part of the time', 
                                c("1"), c("0")) 
#convert Tampon.use.1mth from character into factor
total$Tampon.use.1mth <- factor(total$Tampon.use.1mth)
summary(total$Tampon.use.1mth)
##############################################################

#Substance use
#change to never-ever cats
data$Substance.Use[data$Substance.Use >0] <- '1' #emcompass those who use past or currently

#convert Substance.Use from character into factor
data$Substance.Use <- factor(data$Substance.Use)

##############################################################

#smoking
summary(total$smoker..current.or.in.past...y.1..n.0.)
summary(total$smoker..current.or.in.past...y.1..n.0.cat)
#condense to currently smoking or not
total$smoking.current <- ifelse(total$smoker..current.or.in.past...y.1..n.0. == 'current', 
                                c("1"), c("0")) 
#convert Substance.Use from character into factor
total$smoking.current <- factor(total$smoking.current)
summary(total$smoking.current)