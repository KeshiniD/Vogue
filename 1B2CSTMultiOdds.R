#call data
total <- read.csv(file.path("1B2metabac_condensed.csv"))

#need to be factors if wish to treat like categories
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

#CSTI MultiOdds
#p<0.3; 7 variables (gets messy after 3 variables)
mylogit <- glm(formula = CSTI ~ Genwarts.ever + GenHerpes.ever + Feminine.products.48hrs + 
                 Contraception.none + Yeast..2months. + Chlamydia.ever + Feminine.products, 
               data=total, family = binomial(link = "logit"))
summary(mylogit)

#p<0.2; 4 variables
mylogit <- glm(formula = CSTI ~ Genwarts.ever + GenHerpes.ever + Feminine.products.48hrs + 
                 Contraception.none, data=total, family = binomial(link = "logit"))
summary(mylogit)

#CSTII MultiOdds
#p<0.2; 2 variables
mylogit <- glm(formula = CSTII ~ Yeast..year. + Yeast..lifetime., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#CSTIII MultiOdds
#p<0.1; 3 variables (messy after 2 variables)
mylogit <- glm(formula = CSTIII ~ Abnormal.discharge.48hrs + UTI.ever + smoking.current, 
               data=total, family = binomial(link = "logit"))
summary(mylogit)

#p<0.2; 7 variables
mylogit <- glm(formula = CSTIII ~ Abnormal.discharge.48hrs + UTI.ever + smoking.current + 
                 BMI.cat + Pregnancy.cat + BV..number.of.episodes.2.months. + 
                 Contraception.none, data=total, family = binomial(link = "logit"))
summary(mylogit)

#CSTIVA Multiodds
#p<0.1; 2 variables
mylogit <- glm(formula = CSTIVA ~ Abnormal.odor.2wks + Abnormal.odor.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#p<0.2; 7 variables (messy after 6 variables)
mylogit <- glm(formula = CSTIVA ~ Abnormal.odor.2wks + Abnormal.odor.48hrs + 
                 Freq.sex.toy.use.cat + Yeast..lifetime. + Symptom.pain + 
                 probiotics.2.months + Feminine.products.48hrs, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#CSTIVC Multiodds
#p<0.1; 4 variables
mylogit <- glm(formula = CSTIVC ~ contraception.H + BV..number.of.episodes.2.months. + 
                 contraception.B.M + BV..number.of.episodes.year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#p<0.2; 6 variables
mylogit <- glm(formula = CSTIVC ~ contraception.H + BV..number.of.episodes.2.months. + 
                 contraception.B.M + BV..number.of.episodes.year. + Yeast..year. + 
                 Yeast..lifetime., data=total, family = binomial(link = "logit"))
summary(mylogit)

#CSTIVD Multiodds
#p<0.4; 2 variables
mylogit <- glm(formula = CSTIVD ~ Genwarts.ever + Yeast..year., data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#p<0.5; 4 variables
mylogit <- glm(formula = CSTIVD ~ Genwarts.ever + Yeast..year. + 
                 Irritation.Discomfort.2wks + Other.Symptoms.2wks, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)

#CST_L Multiodds
#p<0.1; 3 variables (messy after 2 variables)
mylogit <- glm(formula = CST_L ~ BV..number.of.episodes.2.months. + Contraception.none + 
                 Tampon.Use.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)

#p<0.2; 7 variables (messy after 2 variables)
mylogit <- glm(formula = CST_L ~ BV..number.of.episodes.2.months. + Contraception.none + 
                 Tampon.Use.cat + Abnormal.odor.48hrs + contraception.H + 
                 Freq.oral.sex.cat + Abnormal.odor.2wks, data=total, family = binomial(link = "logit"))
summary(mylogit)

#CST_NL Multiodds
#p<0.1; 3 variables (messy after 2 variables)
mylogit <- glm(formula = CST_NL ~ BV..number.of.episodes.2.months. + Contraception.none + 
                 Tampon.Use.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)

#p<0.2; 7 variables (messy after 2 variables)
mylogit <- glm(formula = CST_NL ~ BV..number.of.episodes.2.months. + Contraception.none + 
                 Tampon.Use.cat + Abnormal.odor.48hrs + contraception.H + 
                 Freq.oral.sex.cat + Abnormal.odor.2wks, data=total, family = binomial(link = "logit"))
summary(mylogit)

###########################################################################
#Feb-22-16
#multivariate based on fishers and t-test (glm actually Feb-26-16)
total <- read.csv(file.path("1B2metabac_condensedv2.csv"))
#remove random columns
total$X  <-  NULL
total$X.1  <-  NULL
total$X.2  <-  NULL
total$X.3  <-  NULL

#significant based on p<0.1
#CSTI, III, IVD #only one variable w/significant

#CSTIVA
mylogit <- glm(formula = CSTIVA ~ Abnormal.odor.2wks + 
                 Abnormal.odor.48hrs, data=total, family = binomial(link = "logit"))
summary(mylogit)

#CSTIVC
mylogit <- glm(formula = CSTIVC ~ Contraception.none + BV..number.of.episodes.2.months. + 
                 contraception.H + contraception.B.M + BV..number.of.episodes.year., data=total, family = binomial(link = "logit"))
summary(mylogit)



