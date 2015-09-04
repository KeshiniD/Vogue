#call data
total <- read.csv(file.path("1B2metabac_condensed.csv"))
#variable names
colnames(total)

#need to be factors if wish to treat like categories
total$Nugent.score.cat <- factor(total$Nugent.score.cat)
total$Amsels.cat <- factor(total$Amsels.cat)
total$Age.cat <- factor(total$Age.cat)
total$BMI.cat <- factor(total$BMI.cat)
total$Ethnicity.cat <- factor(total$Ethnicity.cat)
total$Antimicrobial.Use..y.1..n.0. <- factor(total$Antimicrobial.Use..y.1..n.0.)
total$X.Non..Prescription..y.1..n.0. <- factor(total$X.Non..Prescription..y.1..n.0.)
total$Freq.of.Menstrual.Period.cat <- factor(total$Freq.of.Menstrual.Period.cat)
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

#CSTI 
#Demographics
mylogit <- glm(formula = CSTI ~ Age.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ BMI.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Ethnicity.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#episodes of BV
mylogit <- glm(formula = CSTI ~ BV..number.of.episodes.2.months., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ BV..number.of.episodes.year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ BV..number.of.episodes.lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#episodes of yeast
mylogit <- glm(formula = CSTI ~ Yeast..2months., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Yeast..year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Yeast..lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#genital infections
mylogit <- glm(formula = CSTI ~ UTI.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Chlamydia.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Genwarts.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Trich.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ GenHerpes.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
 
#meds
mylogit <- glm(formula = CSTI ~ Antimicrobial.Use..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ X.Non..Prescription..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ probiotics.2.months, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#symptoms
mylogit <- glm(formula = CSTI ~ Presence.Symptoms.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Abnormal.discharge.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Abnormal.odor.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Irritation.Discomfort.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Other.Symptoms.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Presence.Symptoms.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Abnormal.discharge.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Abnormal.odor.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Irritation.Discomfort.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Other.Symptoms.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Symptom.pain, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Sexual Activity
mylogit <- glm(formula = CSTI ~ Vaginal.intercourse.in.past.48.hours..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Freq.oral.sex.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Freq.anal.sex.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Freq.sex.toy.use.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Sexual.Partners.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Number.partners.in.past.year.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Contraception
mylogit <- glm(formula = CSTI ~ contraception.H, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ contraception.B.M, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ contraception.C.IUD, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Contraception.none, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ condoms.48h, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Pregnancy
mylogit <- glm(formula = CSTI ~ Pregnancy.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Product use
mylogit <- glm(formula = CSTI ~ Feminine.products, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Feminine.products.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Tampon.Use.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ Tampon.use.1mth, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#substance use
mylogit <- glm(formula = CSTI ~ Substance.Use, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTI ~ smoking.current, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#CSTII 
#Demographics
mylogit <- glm(formula = CSTII ~ Age.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ BMI.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Ethnicity.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#episodes of BV
mylogit <- glm(formula = CSTII ~ BV..number.of.episodes.2.months., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ BV..number.of.episodes.year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ BV..number.of.episodes.lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#episodes of yeast
mylogit <- glm(formula = CSTII ~ Yeast..2months., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Yeast..year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Yeast..lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#genital infections
mylogit <- glm(formula = CSTII ~ UTI.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Chlamydia.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Genwarts.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Trich.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ GenHerpes.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#meds
mylogit <- glm(formula = CSTII ~ Antimicrobial.Use..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ X.Non..Prescription..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ probiotics.2.months, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#symptoms
mylogit <- glm(formula = CSTII ~ Presence.Symptoms.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Abnormal.discharge.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Abnormal.odor.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Irritation.Discomfort.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Other.Symptoms.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Presence.Symptoms.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Abnormal.discharge.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Abnormal.odor.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Irritation.Discomfort.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Other.Symptoms.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Symptom.pain, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Sexual Activity
mylogit <- glm(formula = CSTII ~ Vaginal.intercourse.in.past.48.hours..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Freq.oral.sex.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Freq.anal.sex.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Freq.sex.toy.use.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Sexual.Partners.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Number.partners.in.past.year.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Contraception
mylogit <- glm(formula = CSTII ~ contraception.H, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ contraception.B.M, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ contraception.C.IUD, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Contraception.none, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ condoms.48h, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Pregnancy
mylogit <- glm(formula = CSTII ~ Pregnancy.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Product use
mylogit <- glm(formula = CSTII ~ Feminine.products, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Feminine.products.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Tampon.Use.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ Tampon.use.1mth, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#substance use
mylogit <- glm(formula = CSTII ~ Substance.Use, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTII ~ smoking.current, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#CSTIII 
#Demographics
mylogit <- glm(formula = CSTIII ~ Age.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ BMI.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Ethnicity.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#episodes of BV
mylogit <- glm(formula = CSTIII ~ BV..number.of.episodes.2.months., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ BV..number.of.episodes.year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ BV..number.of.episodes.lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#episodes of yeast
mylogit <- glm(formula = CSTIII ~ Yeast..2months., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Yeast..year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Yeast..lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#genital infections
mylogit <- glm(formula = CSTIII ~ UTI.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Chlamydia.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Genwarts.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Trich.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ GenHerpes.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#meds
mylogit <- glm(formula = CSTIII ~ Antimicrobial.Use..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ X.Non..Prescription..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ probiotics.2.months, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#symptoms
mylogit <- glm(formula = CSTIII ~ Presence.Symptoms.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Abnormal.discharge.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Abnormal.odor.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Irritation.Discomfort.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Other.Symptoms.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Presence.Symptoms.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Abnormal.discharge.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Abnormal.odor.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Irritation.Discomfort.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Other.Symptoms.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Symptom.pain, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Sexual Activity
mylogit <- glm(formula = CSTIII ~ Vaginal.intercourse.in.past.48.hours..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Freq.oral.sex.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Freq.anal.sex.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Freq.sex.toy.use.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Sexual.Partners.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Number.partners.in.past.year.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Contraception
mylogit <- glm(formula = CSTIII ~ contraception.H, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ contraception.B.M, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ contraception.C.IUD, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Contraception.none, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ condoms.48h, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Pregnancy
mylogit <- glm(formula = CSTIII ~ Pregnancy.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Product use
mylogit <- glm(formula = CSTIII ~ Feminine.products, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Feminine.products.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Tampon.Use.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ Tampon.use.1mth, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#substance use
mylogit <- glm(formula = CSTIII ~ Substance.Use, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIII ~ smoking.current, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#CSTIVA 
#Demographics
mylogit <- glm(formula = CSTIVA ~ Age.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ BMI.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Ethnicity.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#episodes of BV
mylogit <- glm(formula = CSTIVA ~ BV..number.of.episodes.2.months., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ BV..number.of.episodes.year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ BV..number.of.episodes.lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#episodes of yeast
mylogit <- glm(formula = CSTIVA ~ Yeast..2months., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Yeast..year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Yeast..lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#genital infections
mylogit <- glm(formula = CSTIVA ~ UTI.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Chlamydia.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Genwarts.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Trich.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ GenHerpes.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#meds
mylogit <- glm(formula = CSTIVA ~ Antimicrobial.Use..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ X.Non..Prescription..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ probiotics.2.months, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#symptoms
mylogit <- glm(formula = CSTIVA ~ Presence.Symptoms.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Abnormal.discharge.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Abnormal.odor.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Irritation.Discomfort.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Other.Symptoms.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Presence.Symptoms.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Abnormal.discharge.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Abnormal.odor.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Irritation.Discomfort.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Other.Symptoms.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Symptom.pain, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Sexual Activity
mylogit <- glm(formula = CSTIVA ~ Vaginal.intercourse.in.past.48.hours..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Freq.oral.sex.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Freq.anal.sex.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Freq.sex.toy.use.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Sexual.Partners.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Number.partners.in.past.year.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Contraception
mylogit <- glm(formula = CSTIVA ~ contraception.H, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ contraception.B.M, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ contraception.C.IUD, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Contraception.none, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ condoms.48h, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Pregnancy
mylogit <- glm(formula = CSTIVA ~ Pregnancy.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Product use
mylogit <- glm(formula = CSTIVA ~ Feminine.products, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Feminine.products.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Tampon.Use.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ Tampon.use.1mth, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#substance use
mylogit <- glm(formula = CSTIVA ~ Substance.Use, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVA ~ smoking.current, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#CSTIVC 
#Demographics
mylogit <- glm(formula = CSTIVC ~ Age.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ BMI.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Ethnicity.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#episodes of BV
mylogit <- glm(formula = CSTIVC ~ BV..number.of.episodes.2.months., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ BV..number.of.episodes.year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ BV..number.of.episodes.lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#episodes of yeast
mylogit <- glm(formula = CSTIVC ~ Yeast..2months., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Yeast..year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Yeast..lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#genital infections
mylogit <- glm(formula = CSTIVC ~ UTI.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Chlamydia.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Genwarts.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Trich.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ GenHerpes.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#meds
mylogit <- glm(formula = CSTIVC ~ Antimicrobial.Use..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ X.Non..Prescription..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ probiotics.2.months, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#symptoms
mylogit <- glm(formula = CSTIVC ~ Presence.Symptoms.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Abnormal.discharge.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Abnormal.odor.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Irritation.Discomfort.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Other.Symptoms.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Presence.Symptoms.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Abnormal.discharge.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Abnormal.odor.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Irritation.Discomfort.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Other.Symptoms.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Symptom.pain, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Sexual Activity
mylogit <- glm(formula = CSTIVC ~ Vaginal.intercourse.in.past.48.hours..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Freq.oral.sex.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Freq.anal.sex.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Freq.sex.toy.use.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Sexual.Partners.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Number.partners.in.past.year.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Contraception
mylogit <- glm(formula = CSTIVC ~ contraception.H, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ contraception.B.M, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ contraception.C.IUD, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Contraception.none, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ condoms.48h, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Pregnancy
mylogit <- glm(formula = CSTIVC ~ Pregnancy.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Product use
mylogit <- glm(formula = CSTIVC ~ Feminine.products, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Feminine.products.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Tampon.Use.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ Tampon.use.1mth, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#substance use
mylogit <- glm(formula = CSTIVC ~ Substance.Use, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVC ~ smoking.current, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#CSTIVD 
#Demographics
mylogit <- glm(formula = CSTIVD ~ Age.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ BMI.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Ethnicity.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#episodes of BV
mylogit <- glm(formula = CSTIVD ~ BV..number.of.episodes.2.months., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ BV..number.of.episodes.year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ BV..number.of.episodes.lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#episodes of yeast
mylogit <- glm(formula = CSTIVD ~ Yeast..2months., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Yeast..year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Yeast..lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#genital infections
mylogit <- glm(formula = CSTIVD ~ UTI.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Chlamydia.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Genwarts.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Trich.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ GenHerpes.ever, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#meds
mylogit <- glm(formula = CSTIVD ~ Antimicrobial.Use..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ X.Non..Prescription..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ probiotics.2.months, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#symptoms
mylogit <- glm(formula = CSTIVD ~ Presence.Symptoms.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Abnormal.discharge.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Abnormal.odor.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Irritation.Discomfort.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Other.Symptoms.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Presence.Symptoms.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Abnormal.discharge.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Abnormal.odor.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Irritation.Discomfort.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Other.Symptoms.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Symptom.pain, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Sexual Activity
mylogit <- glm(formula = CSTIVD ~ Vaginal.intercourse.in.past.48.hours..y.1..n.0., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Freq.oral.sex.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Freq.anal.sex.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Freq.sex.toy.use.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Sexual.Partners.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Number.partners.in.past.year.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Contraception
mylogit <- glm(formula = CSTIVD ~ contraception.H, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ contraception.B.M, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ contraception.C.IUD, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Contraception.none, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ condoms.48h, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Pregnancy
mylogit <- glm(formula = CSTIVD ~ Pregnancy.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#Product use
mylogit <- glm(formula = CSTIVD ~ Feminine.products, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Feminine.products.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Tampon.Use.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ Tampon.use.1mth, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#substance use
mylogit <- glm(formula = CSTIVD ~ Substance.Use, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = CSTIVD ~ smoking.current, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)