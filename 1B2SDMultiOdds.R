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

#Multivariate for Shannon's Diversity
mylogit <- glm(formula = Shannon.s.Diversity ~ Chlamydia.ever + 
                 Yeast..2months. + Antimicrobial.Use..y.1..n.0. + 
                 Tampon.Use.cat + X.Non..Prescription..y.1..n.0., data=total, 
               family = poisson(link = "log"))
summary(mylogit)
confint(mylogit)#confidence intervals 
#can exponentiate with exp() infront

#cannot convert glm into data.frame but use below to get data
results_df <-summary.glm(mylogit)$coefficients #can write this to file

##############################################################################
#Feb-26-16
#multivariate based on fishers and t-test
total <- read.csv(file.path("1B2metabac_condensedv2.csv"))
#remove random columns
total$X  <-  NULL
total$X.1  <-  NULL
total$X.2  <-  NULL
total$X.3  <-  NULL

#significant based on p<0.1
mylogit <- glm(formula = Shannon.s.Diversity ~ Chlamydia.ever + 
                 Yeast..2months. + Antimicrobial.Use..y.1..n.0. + 
                 Vaginal.intercourse.in.past.48.hours..y.1..n.0. + 
                 Freq.sex.toy.use.cat, data=total, 
               family = poisson(link = "log"))
summary(mylogit)
confint(mylogit)

#####################################################################
#Feb-26-16
#plot for fisher's significant variables
SD <- read.csv(file="SD_uni_Fishers.csv")

SD$colour <- ifelse(SD$Estimate < 0, "negative","positive")
SD$hjust <- ifelse(SD$Estimate > 0, 1.3, -0.3)
ggplot(SD,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)")