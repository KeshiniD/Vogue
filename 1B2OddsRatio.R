#odds ratio
## might needs epitools package

#load packages
library(vegan)
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)
library(assertthat)
library(entropart)
library(epitools)
library(ggtree)
library(PredictABEL) #for adjusted odds ratio

#call for entire 1B2 data
total <- read.csv(file.path("1B2metbac_v2.csv"))

#rename headers
#rename funtion does not work with spaces unless quoted
#did not use yet
total <- dplyr::rename(total,
                      #"Lactobacillus crispatus" = Lactobacillus.crispatus, 
                      #"Lactobacillus iners" = Lactobacillus.iners, 
                      #"Lactobacillus gasseri" = Lactobacillus.gasseri, 
                      #"Lactobacillus jensenii" = Lactobacillus.jensenii, 
                      #"Gardnerella vaginalis Group C" = Gardnerella.vaginalis.Group.C, 
                      #"Gardnerella vaginalis Group A" = Gardnerella.vaginalis.Group.A,
                      #"Gardnerella vaginalis Group B" = Gardnerella.vaginalis.Group.B,
                      #"Gardnerella vaginalis Group D" = Gardnerella.vaginalis.Group.D,
                      "Megasphaera.sp.genomosp.type.1" = Megasphaera.sp..genomosp..type.1, 
                      #"Escherichia coli" = Escherichia.coli,"Prevotella timonensis" = Prevotella.timonensis, 
                      "Clostridia.sp.BVAB2" = Clostridia.sp..BVAB2, 
                      "Clostridium.genomosp.BVAB3" = Clostridium.genomosp..BVAB3, 
                      #"Atopobium vaginae" = Atopobium.vaginae, "Other Clostridia" = Other.Clostridia, 
                      #"Other Bacteroidetes" = Other.Bacteroidetes, "Other Proteobacteria" = Other.Proteobacteria,
                      #"Other Actinobacteria" = Other.Actinobacteria, "Other Firmicutes" = Other.Firmicutes, 
                      "Nugent Score" = Nugent.score, "Amsel's Criteria" = Amsels, 
                      "Marital Status" = Marital.Status, "Highest Education Level" = Highest.Education.Level, 
                      "Current or chronic conditions" = Current.or.chronic.conditions...y.1..n.0., 
                      "History of Genital Infections" = Genital.Infections..y.1..n.0., 
                      "Number of BV episodes (past 2 months)" = BV..number.of.episodes.2.months., 
                      "Number of BV episodes (past year)" = BV..number.of.episodes.year., 
                      "Number of BV episodes (lifetime)" = BV..number.of.episodes.lifetime., 
                      "Number of Yeast episodes (past two months)" = Yeast..2months., 
                      "Number of Yeast episodes (past year)" = Yeast..year., 
                      "Number of Yeast episodes (lifetime)" = Yeast..lifetime.,
                      "Number of UTI episodes (past two months)" = UTI..2.months., 
                      "Number of UTI episodes (past year)" = UTI..year., 
                      "Number of UTI episodes (lifetime)" = UTI..lifetime., 
                      "Number of Trichomoniasis episodes (past two months)" = Trich..2.months.,
                      "Number of Trichomoniasis episodes (past year)" = Trich..year., 
                      "Number of Trichomoniasis episodes (lifetime)" = Trich..lifetime., 
                      "Number of Genital Warts episodes (past two months)" = Genital.Warts..2months., 
                      "Number of Gential warts episodes (past year)" = Genital.Warts..year., 
                      "Number of Genital Warts episodes (lifetime)" = Genital.Warts..lifetime., 
                      "Number of Genital Herpes episodes (past two months)" = Genital.Herpes..2months., 
                      "Number of Gential Herpes episodes (past year)" = Genital.Herpes..year., 
                      "Number of Genital Herpes episodes (lifetime)" = Genital.Herpes..lifetime.,  
                      "Number of Chlamydia episodes (past two months)" = Chlamydia..2.months., 
                      "Number of Chlamydia episodes (past year)" = Chlamydia..year., 
                      "Number of Chlamydia episodes (lifetime)" = Chlamydia..lifetime., 
                      "History of Gonorrhea" = Gonorrhea, "History of Syphilis" = Syphillis, 
                      "Antimicrobial Use (past 3 months)" = Antimicrobial.Use..y.1..n.0., 
                      "(Non)Prescription Use (past 2 months)" = X.Non..Prescription..y.1..n.0., 
                      "Frequency of Menstrual Period" = Freq.of.Menstrual.Period, "Tampon Usage" = Tampon.Use, 
                      "Pregnancy History (Gravida)" = Pregnancy.History..g., 
                      "Pregnancy History (Term)" = Pregnancy.History..term., 
                      "Pregnancy History (Spontaneous Abortion)" = Pregnancy.History..sa., 
                      "Pregnancy History (Terminated Abortion)" = Pregnancy.History..ta., 
                      "Pregnancy History (Livebirth)" = Pregnancy.History..l., 
                      "Pregnancy History (Preterm)" = Pregnancy.History..p., 
                      "Presence of Symptoms" = Symptoms..y.1..n.0., 
                      "Abnormal Discharge" = abnormal.discharge..y.1..n.0., 
                      "Abnormal Odor" = abnormal.odor..y.1..n.0., 
                      "Irritation/Discomfort" = irritation.or.discomfort..y.1..n.0., 
                      "Other symptoms" = other, 
                      "How often pain experienced during vaginal intercourse" = pain.during.vaginal.intercourse..how.often., 
                      "Douche Product Usage" = Use.of.douche.products..y.1..n.0., 
                      "Use in past 48 hours" = Used.in.the.past.48.hours, 
                      "Feminine Hygenie Product Usage" = Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0., 
                      "Form of Contraception" = Form.of.contraception, "Sexual Partners" = Sexual.Partners, 
                      "NUmber of partners (past two months)" = Number.partners.in.past.2.months, 
                      "NUmber of partners (past year)" = Number.partners.in.past.year, 
                      "Vaginal Intercourse (past 48 hours)" = Vaginal.intercourse.in.past.48.hours..y.1..n.0., 
                      "Frequency of Oral Sex" = Freq.oral.sex, 
                      "Oral Sex (past 48 hours)" = oral.sex.in.past.48.hours..y.1..n.0., 
                      "Frequency of Anal Sex" = Freq.anal.sex, 
                      "Anal Sex (past 48 hours)" = anal.sex.in.past.48.hours..y.1..n.0., 
                      "Frequency of Sex Toy Use" = Freq.sex.toy.use, 
                      "Sex Toy Use (past 48 hours)" = use.in.past.48.hours..y.1..n.0., 
                      "Illicit Substance Use" = use.of.drugs..y.1..n.0., "Alcohol Use" = alcohol.use..y.1..n.0., 
                      "Smoking (Current or Past)" = smoker..current.or.in.past...y.1..n.0.) 

#Odds Ratio 
#data has been put into categories
#data has to be in factor form
#not necessarily (only if you want it to be treated at category)
total$Nugent.score.cat <- factor(total$Nugent.score.cat)
total$Amsels.cat <- factor(total$Amsels.cat)
total$Age.cat <- factor(total$Age.cat)
total$BMI.cat <- factor(total$BMI.cat)
total$Ethnicity.cat <- factor(total$Ethnicity.cat)
total$Marital.Status.cat <- factor(total$Marital.Status.cat)
total$Highest.Education.Level.cat <- factor(total$Highest.Education.Level.cat)
total$Current.or.chronic.conditions...y.1..n.0. <- factor(total$Current.or.chronic.conditions...y.1..n.0.)
total$Genital.Infections..y.1..n.0. <- factor(total$Genital.Infections..y.1..n.0.)
total$Antimicrobial.Use..y.1..n.0. <- factor(total$Antimicrobial.Use..y.1..n.0.)
total$X.Non..Prescription..y.1..n.0. <- factor(total$X.Non..Prescription..y.1..n.0.)
total$Freq.of.Menstrual.Period.cat <- factor(total$Freq.of.Menstrual.Period.cat)
total$Tampon.Use.cat <- factor(total$Tampon.Use.cat)
total$Use.of.douche.products..y.1..n.0. <- factor(total$Use.of.douche.products..y.1..n.0.)
total$Used.in.the.past.48.hours <- factor(total$Used.in.the.past.48.hours)
total$Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0. <- factor(total$Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0.)
total$Used.in.past.48.hours <- factor(total$Used.in.past.48.hours)
total$Contraception.cat <- factor(total$Contraception.cat)
total$Sexual.Partners.cat <- factor(total$Sexual.Partners.cat)
total$Vaginal.intercourse.in.past.48.hours..y.1..n.0. <- factor(total$Vaginal.intercourse.in.past.48.hours..y.1..n.0.)
total$Freq.oral.sex.cat <- factor(total$Freq.oral.sex.cat)
total$oral.sex.in.past.48.hours..y.1..n.0. <- factor(total$oral.sex.in.past.48.hours..y.1..n.0.)
total$Freq.anal.sex.cat <- factor(total$Freq.anal.sex.cat)
total$anal.sex.in.past.48.hours..y.1..n.0. <- factor(total$anal.sex.in.past.48.hours..y.1..n.0.)
total$Freq.sex.toy.use.cat <- factor(total$Freq.sex.toy.use.cat)
total$use.in.past.48.hours..y.1..n.0. <- factor(total$use.in.past.48.hours..y.1..n.0.)
total$use.of.drugs..y.1..n.0.cat <- factor(total$use.of.drugs..y.1..n.0.cat)
total$alcohol.use..y.1..n.0.cat <- factor(total$alcohol.use..y.1..n.0.cat)
total$smoker..current.or.in.past...y.1..n.0.cat <- factor(total$smoker..current.or.in.past...y.1..n.0.cat)
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
total$Preg.livebirth.ever <- factor(total$Preg.livebirth.ever)
total$Chlamydia.ever <- factor(total$Chlamydia.ever)
total$Gonorrhea.ever <- factor(total$Gonorrhea.ever)
total$Bac.STI.ever <- factor(total$Bac.STI.ever)
total$Herpes.ever <- factor(total$Herpes.ever)
total$Genwarts.ever <- factor(total$Genwarts.ever)
total$Number.partners.in.past.2.months.cat <- factor(total$Number.partners.in.past.2.months.cat)
total$Number.partners.in.past.year.cat <- factor(total$Number.partners.in.past.year.cat)
total$any.sx.pain <- factor(total$any.sx.pain)
total$sx.pain.50.over <- factor(total$sx.pain.50.over)
total$sx.pain.100 <- factor(total$sx.pain.100)
total$contraception.H <- factor(total$contraception.H)
total$contraception.S.S <- factor(total$contraception.S.S)
total$contraception.S.P <- factor(total$contraception.S.P)
total$contraception.B.M <- factor(total$contraception.B.M)
total$contraception.B.F <- factor(total$contraception.B.F)
total$contraception.C.IUD <- factor(total$contraception.C.IUD)
total$HContr.Progestin.pill <- factor(total$HContr.Progestin.pill)
total$HContr.Combination.pill <- factor(total$HContr.Combination.pill)
total$HContr.nuvaring <- factor(total$HContr.nuvaring)
total$HContr.mirena <- factor(total$HContr.mirena)
total$HContr.depoprovera <- factor(total$HContr.depoprovera)
total$HContr.orthoevra <- factor(total$HContr.orthoevra)
total$contr_type <- factor(total$contr_type)
total$condoms.48h <- factor(total$condoms.48h)
total$probiotics.2.months <- factor(total$probiotics.2.months)
total$weeks.since.LMP.cat <- factor(total$weeks.since.LMP.cat)
total$CST.cat <- factor(total$CST.cat)
total$CST <- factor(total$CST)

#odds ratio code
#na in data is ok
#the variable in first slot needs to be 0,1 factor
mylogit <- glm(formula, data, family = binomial(link = "logit"))
mylogit <- glm(formula, data, family = binomial) 
#gives same results thus far

#error --> fixed; needed to remove categories which only had one factor
mylogit <- glm(formula = Nugent.score.cat ~ Shannon.s.Diversity + Amsels.cat +  
                 Age.cat + BMI.cat + Ethnicity.cat + Marital.Status.cat + 
                 Highest.Education.Level.cat + 
                 Current.or.chronic.conditions...y.1..n.0. + 
                 Genital.Infections..y.1..n.0. + 
                 BV..number.of.episodes.2.months.+ 
                 BV..number.of.episodes.year. + 
                 BV..number.of.episodes.lifetime. + Yeast..2months. + 
                 Yeast..year. + Yeast..lifetime. + UTI..2.months. + 
                 UTI..year. + UTI..lifetime. + Trich..2.months. + 
                 Trich..year. + Trich..lifetime. + Genital.Warts..2months. + 
                 Genital.Warts..year. + Genital.Warts..lifetime. + 
                 Genital.Herpes..2months. + Genital.Herpes..year. + 
                 Genital.Herpes..lifetime. + Chlamydia..2.months. + 
                 Chlamydia..year. + Chlamydia..lifetime. + Gonorrhea + 
                 Syphillis + Antimicrobial.Use..y.1..n.0. + 
                 X.Non..Prescription..y.1..n.0. + 
                 Freq.of.Menstrual.Period.cat + Tampon.Use.cat + 
                 Pregnancy.History..g. + Pregnancy.History..term. + 
                 Pregnancy.History..p. + Pregnancy.History..sa. + 
                 Pregnancy.History..ta. + Pregnancy.History..l. + 
                 Use.of.douche.products..y.1..n.0. + 
                 Used.in.the.past.48.hours + 
                 Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0. + 
                 Used.in.past.48.hours + Sexual.Partners.cat + 
                 Number.partners.in.past.2.months.cat + 
                 Number.partners.in.past.year.cat + 
                 Vaginal.intercourse.in.past.48.hours..y.1..n.0. + 
                 Freq.oral.sex.cat + Freq.anal.sex.cat + Freq.sex.toy.use.cat + 
                 use.of.drugs..y.1..n.0.cat + alcohol.use..y.1..n.0.cat + 
                 smoker..current.or.in.past...y.1..n.0.cat + 
                 Actinobacteria.sp. + Atopobium.vaginae + 
                 Clostridia.sp..BVAB2 + Clostridium.genomosp..BVAB3 + 
                 Escherichia.coli + Gardnerella.vaginalis.Group.A + 
                 Gardnerella.vaginalis.Group.B + 
                 Gardnerella.vaginalis.Group.C + 
                 Gardnerella.vaginalis.Group.D + Klebsiella.pneumoniae + 
                 Lactobacillus.crispatus + Lactobacillus.gasseri + 
                 Lactobacillus.iners + Lactobacillus.jensenii + 
                 Megasphaera.sp..genomosp..type.1 + Other.Actinobacteria + 
                 Other.Bacteria + Other.Bacteroidetes + Other.Clostridium + 
                 Other.Firmicutes + Other.Lactobacillus + Other.Prevotella + 
                 Other.Proteobacteria + Other.Streptococcus + 
                 Prevotella.amnii + Prevotella.timonensis + Streptococcus.devriesei + 
                 How.often.pain.experienced.during.vaginal.intercourse.percentage +
                 Contraception.cat + Presence.Symptoms.2wks + 
                 Abnormal.discharge.2wks + Abnormal.odor.2wks + 
                 Irritation.Discomfort.2wks + Other.Symptoms.2wks + 
                 Presence.Symptoms.48hrs + Irritation.Discomfort.48hrs + 
                 Other.Symptoms.48hrs + Preg.livebirth.ever + Chlamydia.ever + 
                 Bac.STI.ever + Herpes.ever + Genwarts.ever + any.sx.pain + 
                 sx.pain.50.over + sx.pain.100 + contraception.H + 
                 contraception.S.S + contraception.S.P + contraception.B.M + 
                 contraception.C.IUD + HContr.Combination.pill + HContr.mirena + 
                 contr_type + condoms.48h + probiotics.2.months + 
                 days.since.LMP + weeks.since.LMP + weeks.since.LMP.cat + 
                 CST.cat, data = total, family = binomial(link = "logit"))
#overall no associations or statisical significance
#way too many variables and few observations

#these cause error when put with set above but work fine alone :/
mylogit <- glm(formula = Nugent.score.cat ~ Abnormal.odor.48hrs + 
                 Abnormal.discharge.48hrs + HContr.Progestin.pill + 
                 oral.sex.in.past.48.hours..y.1..n.0., data = total, family = binomial(link = "logit"))

#applying this same code to see associations between NUgent.score, Amsels.cat
#and CST.cat, with remainder of variables
#also applying same code going to see individual variables on Nugent.score, Amsels
#and CST

mylogit
confint(mylogit) #CI intervals
exp(cbind(OR = coef(mylogit), confint(mylogit))) #ORs and CIs
exp(coef(mylogit)) #only ORs
summary(mylogit)# for nice table
#cannot convert glm into data.frame but use below to get data
results_df <-summary.glm(mylogit)$coefficients #can write this to file






#Amsels.cat with variables
mylogit <- glm(formula = Amsels.cat ~ Shannon.s.Diversity, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ CST.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Age.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ BMI.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Ethnicity.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Marital.Status.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Highest.Education.Level.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Current.or.chronic.conditions...y.1..n.0., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Genital.Infections..y.1..n.0., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ BV..number.of.episodes.2.months., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ BV..number.of.episodes.year., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ BV..number.of.episodes.lifetime., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Yeast..2months., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Yeast..year., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Yeast..lifetime., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ UTI..2.months., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ UTI..year., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ UTI..lifetime., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Trich..2.months., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Trich..year., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Trich..lifetime., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Genital.Warts..2months., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Genital.Warts..year., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Genital.Warts..lifetime., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Genital.Herpes..2months., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Genital.Herpes..year., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Genital.Herpes..lifetime., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Chlamydia..2.months., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Chlamydia..year., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Chlamydia..lifetime., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Gonorrhea, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Syphillis, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Antimicrobial.Use..y.1..n.0., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ X.Non..Prescription..y.1..n.0., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Freq.of.Menstrual.Period.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Tampon.Use.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Pregnancy.History..g., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Pregnancy.History..term., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Pregnancy.History..p., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Pregnancy.History..sa., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Pregnancy.History..ta., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Pregnancy.History..l., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Use.of.douche.products..y.1..n.0., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Used.in.the.past.48.hours, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Used.in.past.48.hours, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Sexual.Partners.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Number.partners.in.past.2.months.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Number.partners.in.past.year.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Vaginal.intercourse.in.past.48.hours..y.1..n.0., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Freq.oral.sex.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Freq.anal.sex.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Freq.sex.toy.use.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ use.of.drugs..y.1..n.0.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ alcohol.use..y.1..n.0.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ smoker..current.or.in.past...y.1..n.0.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Actinobacteria.sp., data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Atopobium.vaginae, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Clostridia.sp..BVAB2, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Clostridium.genomosp..BVAB3, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Escherichia.coli, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Gardnerella.vaginalis.Group.A, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Gardnerella.vaginalis.Group.B, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Gardnerella.vaginalis.Group.C, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Gardnerella.vaginalis.Group.D, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Klebsiella.pneumoniae, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Lactobacillus.crispatus, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Lactobacillus.gasseri, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Lactobacillus.iners, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Lactobacillus.jensenii, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Megasphaera.sp..genomosp..type.1, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Other.Actinobacteria, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Other.Bacteria, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Other.Bacteroidetes, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Other.Clostridium, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Other.Firmicutes, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Other.Lactobacillus, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Other.Prevotella, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Other.Proteobacteria, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Other.Streptococcus, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Prevotella.amnii, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Streptococcus.devriesei, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ How.often.pain.experienced.during.vaginal.intercourse.percentage, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Contraception.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Presence.Symptoms.2wks, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Abnormal.discharge.2wks, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Abnormal.odor.2wks, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Irritation.Discomfort.2wks, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Other.Symptoms.2wks, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Presence.Symptoms.48hrs, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Irritation.Discomfort.48hrs, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Other.Symptoms.48hrs, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Preg.livebirth.ever, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Chlamydia.ever, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Bac.STI.ever, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Herpes.ever, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Genwarts.ever, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ any.sx.pain, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ sx.pain.50.over, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ sx.pain.100, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ contraception.H, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ contraception.S.S, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ contraception.S.P, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ contraception.B.M, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ contraception.C.IUD, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ HContr.Combination.pill, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ HContr.mirena, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ contr_type, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ condoms.48h, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ probiotics.2.months, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ days.since.LMP, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ weeks.since.LMP, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ weeks.since.LMP.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)
mylogit <- glm(formula = Amsels.cat ~ Nugent.score.cat, data=total, family = binomial(link = "logit"))
summary(mylogit)

mylogit <- glm(formula = Amsels.cat ~ Prevotella.timonensis, data=total, family = binomial(link = "logit"))
mylogit <- glm(formula = Amsels.cat ~ Abnormal.discharge.48hrs, data=total, family = binomial(link = "logit"))
mylogit <- glm(formula = Amsels.cat ~ Abnormal.odor.48hrs, data=total, family = binomial(link = "logit"))
mylogit <- glm(formula = Amsels.cat ~ oral.sex.in.past.48.hours..y.1..n.0., data=total, family = binomial(link = "logit"))
