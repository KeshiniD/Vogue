#call for entire 1B2 data
total <- read.csv(file.path("1B2metabac_v3.csv"))
total2 <- read.csv(file.path("1B2metabac.csv"))
#descriptives
mean(total$Age)
range(total$Age)
sd(total$Age)

mean(total$BMI)
range(total$BMI)
sd(total$BMI)

#
summary(total$Ethnicity)
summary(total$Marital.Status)
summary(total$Highest.Education.Level)
summary(total$Current.or.chronic.conditions...y.1..n.0.)
summary(total$Antimicrobial.Use..y.1..n.0.)
summary(total$X.Non..Prescription..y.1..n.0.)

#
mean(total$Pregnancy.History..g.)
range(total$Pregnancy.History..g.)
sd(total$Pregnancy.History..g.)
summary(total$Preg.livebirth.ever)

#symptoms
summary(total$Presence.Symptoms.2wks)
summary(total$Presence.Symptoms.48hrs)
summary(total$Abnormal.discharge.48hrs)
summary(total$Abnormal.odor.48hrs)
summary(total$Irritation.Discomfort.48hrs)
summary(total$any.sx.pain)
summary(total$Other.Symptoms.48hrs)

#
mean(total$How.often.pain.experienced.during.vaginal.intercourse.percentage, na.rm=TRUE)
range(total$How.often.pain.experienced.during.vaginal.intercourse.percentage, na.rm=TRUE)
sd(total$How.often.pain.experienced.during.vaginal.intercourse.percentage, na.rm=TRUE)

#
summary(total$Contraception)

#genital infections
#BV
mean(total$BV..number.of.episodes.2.months., na.rm=TRUE)
range(total$BV..number.of.episodes.2.months., na.rm=TRUE)
sd(total$BV..number.of.episodes.2.months., na.rm=TRUE)
mean(total$BV..number.of.episodes.year., na.rm=TRUE)
range(total$BV..number.of.episodes.year., na.rm=TRUE)
sd(total$BV..number.of.episodes.year., na.rm=TRUE)
mean(total$BV..number.of.episodes.lifetime., na.rm=TRUE)
range(total$BV..number.of.episodes.lifetime., na.rm=TRUE)
sd(total$BV..number.of.episodes.lifetime., na.rm=TRUE)

#Yeast
mean(total$Yeast..2months., na.rm=TRUE)
range(total$Yeast..2months., na.rm=TRUE)
sd(total$Yeast..2months., na.rm=TRUE)
mean(total$Yeast..year., na.rm=TRUE)
range(total$Yeast..year., na.rm=TRUE)
sd(total$Yeast..year., na.rm=TRUE)
mean(total$Yeast..lifetime., na.rm=TRUE)
range(total$Yeast..lifetime., na.rm=TRUE)
sd(total$Yeast..lifetime., na.rm=TRUE)

#UTI
mean(total$UTI..2.months., na.rm=TRUE)
range(total$UTI..2.months., na.rm=TRUE)
sd(total$UTI..2.months., na.rm=TRUE)
mean(total$UTI..year., na.rm=TRUE)
range(total$UTI..year., na.rm=TRUE)
sd(total$UTI..year., na.rm=TRUE)
mean(total$UTI..lifetime., na.rm=TRUE)
range(total$UTI..lifetime., na.rm=TRUE)
sd(total$UTI..lifetime., na.rm=TRUE)

#
summary(total$Nugent.score.cat)
summary(total$Freq.of.Menstrual.Period)
summary(total$Tampon.Use)
summary(total$Use.of.douche.products..y.1..n.0.)
summary(total$Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0.)
summary(total$use.of.drugs..y.1..n.0.)
summary(total$alcohol.use..y.1..n.0.)
summary(total$smoker..current.or.in.past...y.1..n.0.)
summary(total$Sexual.Partners)

#
mean(total$Number.partners.in.past.2.months, na.rm=TRUE)
range(total$Number.partners.in.past.2.months, na.rm=TRUE)
sd(total$Number.partners.in.past.2.months, na.rm=TRUE)
mean(total$Number.partners.in.past.year, na.rm=TRUE)
range(total$Number.partners.in.past.year, na.rm=TRUE)
sd(total$Number.partners.in.past.year, na.rm=TRUE)

#
summary(total$Vaginal.intercourse.in.past.48.hours..y.1..n.0.)
summary(total$Freq.oral.sex)
summary(total$Freq.anal.sex)
summary(total$Freq.sex.toy.use)

#
summary(total$CST)
