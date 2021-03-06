#call data
total <- read.csv(file.path("1B2metabac_condensed.csv"))
#variable names
colnames(total)

#need to be factors if wish to treat like categories
#some of these are not present in the condensed version and need to be removed
total$Nugent.score.cat <- factor(total$Nugent.score.cat)
total$Amsels.cat <- factor(total$Amsels.cat)
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

#Shannons Diversity is continuous variable
#continious outcome
mylogit <- glm(formula, data, family = poisson(link = "log")) 

#cannot convert glm into data.frame but use below to get data
results_df <-summary.glm(mylogit)$coefficients #can write this to file

#Shannon's Diversity
#Demographics
mylogit <- glm(formula = Shannon.s.Diversity ~ Age.cat, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ BMI.cat, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Ethnicity.cat, data=total, 
               family = poisson(link = "log"))
summary(mylogit)

#episodes of BV
mylogit <- glm(formula = Shannon.s.Diversity ~ BV..number.of.episodes.2.months., data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ BV..number.of.episodes.year., data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ BV..number.of.episodes.lifetime., data=total, 
               family = poisson(link = "log"))
summary(mylogit)

#episodes of yeast
mylogit <- glm(formula = Shannon.s.Diversity ~ Yeast..2months., data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Yeast..year., data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Yeast..lifetime., data=total, 
               family = poisson(link = "log"))
summary(mylogit)

#genital infections
mylogit <- glm(formula = Shannon.s.Diversity ~ UTI.ever, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Chlamydia.ever, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Genwarts.ever, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Trich.ever, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ GenHerpes.ever, data=total, 
               family = poisson(link = "log"))
summary(mylogit)

#meds
mylogit <- glm(formula = Shannon.s.Diversity ~ Antimicrobial.Use..y.1..n.0., data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ X.Non..Prescription..y.1..n.0., data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ probiotics.2.months, data=total, 
               family = poisson(link = "log"))
summary(mylogit)

#symptoms
mylogit <- glm(formula = Shannon.s.Diversity ~ Presence.Symptoms.2wks, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Abnormal.discharge.2wks, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Abnormal.odor.2wks, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Irritation.Discomfort.2wks, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Other.Symptoms.2wks, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Presence.Symptoms.48hrs, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Abnormal.discharge.48hrs, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Abnormal.odor.48hrs, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Irritation.Discomfort.48hrs, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Other.Symptoms.48hrs, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Symptom.pain, data=total, 
               family = poisson(link = "log"))
summary(mylogit)

#Sexual Activity
mylogit <- glm(formula = Shannon.s.Diversity ~ Vaginal.intercourse.in.past.48.hours..y.1..n.0., data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Freq.oral.sex.cat, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Freq.anal.sex.cat, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Freq.sex.toy.use.cat, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Sexual.Partners.cat, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Number.partners.in.past.year.cat, data=total, 
               family = poisson(link = "log"))
summary(mylogit)

#Contraception
mylogit <- glm(formula = Shannon.s.Diversity ~ contraception.H, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ contraception.B.M, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ contraception.C.IUD, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Contraception.none, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ condoms.48h, data=total, 
               family = poisson(link = "log"))
summary(mylogit)

#Pregnancy
mylogit <- glm(formula = Shannon.s.Diversity ~ Pregnancy.cat, data=total, 
               family = poisson(link = "log"))
summary(mylogit)

#Product use
mylogit <- glm(formula = Shannon.s.Diversity ~ Feminine.products, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Feminine.products.48hrs, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Tampon.Use.cat, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Tampon.use.1mth, data=total, 
               family = poisson(link = "log"))
summary(mylogit)

#substance use
mylogit <- glm(formula = Shannon.s.Diversity ~ Substance.Use, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ smoking.current, data=total, 
               family = poisson(link = "log"))
summary(mylogit)



#Confidence Intervals
confint(mylogit) #CI intervals (log)
exp(confint(mylogit))  #CI intervals (exponentiated)

#Demographics
mylogit <- glm(formula = Shannon.s.Diversity ~ Age.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ BMI.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Ethnicity.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)

#episodes of BV
mylogit <- glm(formula = Shannon.s.Diversity ~ BV..number.of.episodes.2.months., data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ BV..number.of.episodes.year., data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ BV..number.of.episodes.lifetime., data=total, 
               family = poisson(link = "log"))
confint(mylogit)

#episodes of yeast
mylogit <- glm(formula = Shannon.s.Diversity ~ Yeast..2months., data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Yeast..year., data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Yeast..lifetime., data=total, 
               family = poisson(link = "log"))
confint(mylogit)

#genital infections
mylogit <- glm(formula = Shannon.s.Diversity ~ UTI.ever, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Chlamydia.ever, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Genwarts.ever, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Trich.ever, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ GenHerpes.ever, data=total, 
               family = poisson(link = "log"))
confint(mylogit)

#meds
mylogit <- glm(formula = Shannon.s.Diversity ~ Antimicrobial.Use..y.1..n.0., data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ X.Non..Prescription..y.1..n.0., data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ probiotics.2.months, data=total, 
               family = poisson(link = "log"))
confint(mylogit)

#symptoms
mylogit <- glm(formula = Shannon.s.Diversity ~ Presence.Symptoms.2wks, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Abnormal.discharge.2wks, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Abnormal.odor.2wks, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Irritation.Discomfort.2wks, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Other.Symptoms.2wks, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Presence.Symptoms.48hrs, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Abnormal.discharge.48hrs, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Abnormal.odor.48hrs, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Irritation.Discomfort.48hrs, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Other.Symptoms.48hrs, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Symptom.pain, data=total, 
               family = poisson(link = "log"))
confint(mylogit)

#Sexual Activity
mylogit <- glm(formula = Shannon.s.Diversity ~ Vaginal.intercourse.in.past.48.hours..y.1..n.0., data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Freq.oral.sex.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Freq.anal.sex.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Freq.sex.toy.use.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Sexual.Partners.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Number.partners.in.past.year.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)

#Contraception
mylogit <- glm(formula = Shannon.s.Diversity ~ contraception.H, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ contraception.B.M, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ contraception.C.IUD, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Contraception.none, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ condoms.48h, data=total, 
               family = poisson(link = "log"))
confint(mylogit)

#Pregnancy
mylogit <- glm(formula = Shannon.s.Diversity ~ Pregnancy.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)

#Product use
mylogit <- glm(formula = Shannon.s.Diversity ~ Feminine.products, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Feminine.products.48hrs, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Tampon.Use.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ Tampon.use.1mth, data=total, 
               family = poisson(link = "log"))
confint(mylogit)

#substance use
mylogit <- glm(formula = Shannon.s.Diversity ~ Substance.Use, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
mylogit <- glm(formula = Shannon.s.Diversity ~ smoking.current, data=total, 
               family = poisson(link = "log"))
confint(mylogit)

#############################################################################
#Dec-18-2015
#redo odds for condensed variables

#call for entire 1B2 data
total <- read.csv(file.path("1B2metabac_condensedv2.csv"))
#remove random columns
total$X  <-  NULL
total$X.1  <-  NULL
total$X.2  <-  NULL
total$X.3  <-  NULL

#load packages frmo above and factors including those below
total$BMI.under.cat <- factor(total$BMI.under.cat)
total$BMI.over.cat <- factor(total$BMI.over.cat)
total$BV.2.months.cat <- factor(total$BV.2.months.cat)
total$BV.year.cat <- factor(total$BV.year.cat)
total$BV.lifetime.cat <- factor(total$BV.lifetime.cat)
total$Yeast.2.months.cat <- factor(total$Yeast.2.months.cat)
total$Yeast.year.cat <- factor(total$Yeast.year.cat)
total$Yeast.lifetime.cat <- factor(total$Yeast.lifetime.cat)

##########################################################################
#Demographics
#Age
mylogit <- glm(formula = Shannon.s.Diversity ~ Age.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
summary(mylogit)

#BMI
mylogit <- glm(formula = Shannon.s.Diversity ~ BMI.under.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
summary(mylogit)

mylogit <- glm(formula = Shannon.s.Diversity ~ BMI.over.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
summary(mylogit)

#Ethnicity 
mylogit <- glm(formula = Shannon.s.Diversity ~ Ethnicity.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
summary(mylogit)

#BV episodes
mylogit <- glm(formula = Shannon.s.Diversity ~ BV.2.months.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
summary(mylogit)

mylogit <- glm(formula = Shannon.s.Diversity ~ BV.year.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
summary(mylogit)

mylogit <- glm(formula = Shannon.s.Diversity ~ BV.lifetime.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
summary(mylogit)

#Yeast episodes
mylogit <- glm(formula = Shannon.s.Diversity ~ Yeast.2.months.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
summary(mylogit)

mylogit <- glm(formula = Shannon.s.Diversity ~ Yeast.year.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
summary(mylogit)

mylogit <- glm(formula = Shannon.s.Diversity ~ Yeast.lifetime.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
summary(mylogit)

#Frequency of Tampon Use
mylogit <- glm(formula = Shannon.s.Diversity ~ Tampon.Use.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
summary(mylogit)

#Cat. for number of sexual partners in the last year
mylogit <- glm(formula = Shannon.s.Diversity ~ Number.partners.in.past.year.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
summary(mylogit)

#Frequency of Oral Sex
mylogit <- glm(formula = Shannon.s.Diversity ~ Freq.oral.sex.cat, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
summary(mylogit)

#Substance use
mylogit <- glm(formula = Shannon.s.Diversity ~ Substance.Use, data=total, 
               family = poisson(link = "log"))
confint(mylogit)
summary(mylogit)