#call for entire 1B2 data
total <- read.csv(file.path("1B2metbac_v2.csv"))
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
mean(total$BV..number.of.episodes.2.months., na.rm=TRUE)
range(total$BV..number.of.episodes.2.months., na.rm=TRUE)
sd(total$BV..number.of.episodes.2.months., na.rm=TRUE)
mean(total$BV..number.of.episodes.year., na.rm=TRUE)
range(total$BV..number.of.episodes.year., na.rm=TRUE)
sd(total$BV..number.of.episodes.year., na.rm=TRUE)