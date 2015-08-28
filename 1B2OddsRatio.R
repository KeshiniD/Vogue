#odds ratio
## needs epitools package

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
                 Freq.oral.sex.cat + oral.sex.in.past.48.hours..y.1..n.0. + 
                 Freq.anal.sex.cat + Freq.sex.toy.use.cat + 
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
                 Prevotella.amnii + Streptococcus.devriesei + 
                 How.often.pain.experienced.during.vaginal.intercourse.percentage +
                 Contraception.cat + Presence.Symptoms.2wks + 
                 Abnormal.discharge.2wks + Abnormal.odor.2wks + 
                 Irritation.Discomfort.2wks + Other.Symptoms.2wks + 
                 Presence.Symptoms.48hrs + Abnormal.discharge.48hrs + 
                 Abnormal.odor.48hrs + Irritation.Discomfort.48hrs + 
                 Other.Symptoms.48hrs + Preg.livebirth.ever + 
                 Chlamydia.ever + Bac.STI.ever + 
                 Herpes.ever + Genwarts.ever + any.sx.pain + 
                 sx.pain.50.over + sx.pain.100 + contraception.H + 
                 contraception.S.S + contraception.S.P + contraception.B.M + 
                 contraception.C.IUD + HContr.Progestin.pill + 
                 HContr.Combination.pill + HContr.mirena + contr_type + 
                 condoms.48h + probiotics.2.months + days.since.LMP + weeks.since.LMP + 
                 weeks.since.LMP.cat, data = total, family = binomial)
#to fix error
mylogit <- glm(formula = Nugent.score.cat ~ , data = total, family = binomial(link = "logit"))



mylogit
confint(mylogit) #CI intervals
exp(cbind(OR = coef(mylogit), confint(mylogit))) #ORs and CIs
exp(coef(mylogit)) #only ORs
summary(mylogit)# for really nice table
#cannot convert glm into data.frame

#2x2contingency table
a <- xtabs(~Nugent.score + Ethnicity , data = total)
kable(a)
#3x3 table
a <- xtabs(~Nugent.score + Ethnicity + Marital.Status , data = total)
a <- as.data.frame(a)

