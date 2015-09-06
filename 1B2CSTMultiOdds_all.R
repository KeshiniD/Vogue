#mutlivariate model using all variables; not selecting based on pvalue
#CSTI
#Trich.ever removed; code breaks otherwise
mylogit <- glm(formula = CSTI ~ Age.cat + BMI.cat + Ethnicity.cat + 
                 BV..number.of.episodes.2.months. + 
                 BV..number.of.episodes.year. + 
                 BV..number.of.episodes.lifetime. + Yeast..2months. + Yeast..year. + 
                 Yeast..lifetime. + UTI.ever + Chlamydia.ever + Genwarts.ever + 
                 GenHerpes.ever + Antimicrobial.Use..y.1..n.0. + 
                 X.Non..Prescription..y.1..n.0. + probiotics.2.months + 
                 Presence.Symptoms.2wks + Abnormal.discharge.2wks + 
                 Abnormal.odor.2wks + Irritation.Discomfort.2wks + 
                 Other.Symptoms.2wks + Presence.Symptoms.48hrs + 
                 Abnormal.discharge.48hrs + Abnormal.odor.48hrs + 
                 Irritation.Discomfort.48hrs + Other.Symptoms.48hrs + Symptom.pain + 
                 Vaginal.intercourse.in.past.48.hours..y.1..n.0. + 
                 Freq.oral.sex.cat +  Freq.anal.sex.cat + Freq.sex.toy.use.cat + 
                 Sexual.Partners.cat + Number.partners.in.past.year.cat + 
                 contraception.H + contraception.B.M + contraception.C.IUD + 
                 Contraception.none + condoms.48h + Pregnancy.cat + 
                 Feminine.products + Feminine.products.48hrs + Tampon.Use.cat + 
                 Tampon.use.1mth + Substance.Use + smoking.current, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
results_dfI <-summary.glm(mylogit)$coefficients
View(results_dfI)
#only processes first 18 variables
#select first 18 variables based on pvalue

#CSTI
#Trich.ever removed; code breaks otherwise
#p<0.5668
mylogit <- glm(formula = CSTI ~ Genwarts.ever + GenHerpes.ever + 
                 Feminine.products.48hrs + Contraception.none + Yeast..2months. + 
                 Chlamydia.ever + Feminine.products + UTI.ever + Tampon.use.1mth + 
                 Pregnancy.cat + Abnormal.discharge.48hrs + Irritation.Discomfort.48hrs + 
                 Age.cat + Other.Symptoms.2wks + Freq.anal.sex.cat + 
                 X.Non..Prescription..y.1..n.0. + condoms.48h + Abnormal.discharge.2wks, 
               data=total, family = binomial(link = "logit"))
summary(mylogit)
results_dfI <-summary.glm(mylogit)$coefficients
View(results_dfI)

#CSTII
#Trich.ever removed; breaks code otherwise
#p<0.99709
mylogit <- glm(formula = CSTII ~ Yeast..lifetime. + Yeast..year. + probiotics.2.months + 
                 Other.Symptoms.2wks + Freq.anal.sex.cat + UTI.ever + 
                 Antimicrobial.Use..y.1..n.0. + X.Non..Prescription..y.1..n.0. + 
                 Irritation.Discomfort.2wks + Presence.Symptoms.48hrs + 
                 Abnormal.odor.48hrs + Symptom.pain +  
                 Vaginal.intercourse.in.past.48.hours..y.1..n.0. + Freq.sex.toy.use.cat + 
                 Feminine.products + Tampon.use.1mth + Sexual.Partners.cat + 
                 contraception.C.IUD, data=total, family = binomial(link = "logit"))
summary(mylogit)
results_dfII <-summary.glm(mylogit)$coefficients
View(results_dfII)

#CSTIII
#Trich removed; breaks code
#p<0.437
mylogit <- glm(formula = CSTIII ~ Abnormal.discharge.48hrs + UTI.ever + smoking.current + 
                 BMI.cat + Pregnancy.cat + BV..number.of.episodes.2.months. + 
                 Contraception.none + Symptom.pain + Abnormal.discharge.2wks + 
                 contraception.H + Irritation.Discomfort.48hrs + 
                 BV..number.of.episodes.year. + Abnormal.odor.2wks + 
                 Other.Symptoms.48hrs + Substance.Use + Ethnicity.cat + Age.cat, 
               data=total, family = binomial(link = "logit"))
summary(mylogit)
results_dfIII <-summary.glm(mylogit)$coefficients
View(results_dfIII) 
#ethnicitysubstance/age mucking with the 18 variables; need certain cat from these variables not all

#CSTIVA
#Trich.ever removed; code breaks otherwise
mylogit <- glm(formula = CSTIVA ~ Abnormal.odor.2wks + Abnormal.odor.48hrs + 
                 Freq.sex.toy.use.cat + Yeast..lifetime. + Symptom.pain + 
                 probiotics.2.months + Feminine.products.48hrs + UTI.ever + 
                 Chlamydia.ever + Feminine.products + Yeast..year. + 
                 Presence.Symptoms.48hrs + Abnormal.discharge.48hrs + Yeast..2months. + 
                 Freq.oral.sex.cat + Age.cat + Tampon.Use.cat + 
                 Number.partners.in.past.year.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
results_dfIVA <-summary.glm(mylogit)$coefficients
View(results_dfIVA)
#oral.sex/tampon use/age mucking with the 18 variables; need certain cat from these variables not all

#CSTIVC
#Trich.ever removed; code breaks otherwise
mylogit <- glm(formula = CSTIVC ~ contraception.H + BV..number.of.episodes.2.months. + 
                 contraception.B.M + BV..number.of.episodes.year. + Yeast..year. + 
                 Yeast..lifetime. + Symptom.pain + Abnormal.discharge.2wks + 
                 Presence.Symptoms.48hrs + Abnormal.discharge.48hrs + 
                 Irritation.Discomfort.2wks + BV..number.of.episodes.lifetime. + 
                 Vaginal.intercourse.in.past.48.hours..y.1..n.0. + Tampon.use.1mth + 
                 UTI.ever + Tampon.Use.cat + BMI.cat + Age.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
results_dfIVC <-summary.glm(mylogit)$coefficients
View(results_dfIVC)
#certain variables mucking with the 18 variables; need certain cat from these variables not all

#CSTIVD
#Trich.ever removed; code breaks otherwise
mylogit <- glm(formula = CSTIVD ~ Genwarts.ever + Yeast..year. + 
                 Irritation.Discomfort.2wks + Other.Symptoms.2wks + 
                 Other.Symptoms.48hrs + BV..number.of.episodes.2.months. + 
                 Irritation.Discomfort.48hrs + BV..number.of.episodes.lifetime. + 
                 BV..number.of.episodes.year. + UTI.ever + Yeast..lifetime. + 
                 Yeast..2months. + GenHerpes.ever + Abnormal.discharge.2wks + 
                 Ethnicity.cat + Pregnancy.cat + Tampon.Use.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)
results_dfIVD <-summary.glm(mylogit)$coefficients
View(results_dfIVD)
#certain variables mucking with the 18 variables; need certain cat from these variables not all

#write to file
write.csv(results_dfI, "CSTI_multi_18.csv") 
write.csv(results_dfII, "CSTII_multi_18.csv") 
write.csv(results_dfIII, "CSTIII_multi_18.csv") 
write.csv(results_dfIVA, "CSTIVA_multi_18.csv") 
write.csv(results_dfIVC, "CSTIVC_multi_18.csv") 
write.csv(results_dfIVD, "CSTIVD_multi_18.csv") 

