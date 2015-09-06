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
