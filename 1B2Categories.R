#call for entire 1B2 data
total <- read.csv(file.path("1B2metabac.csv"))

#Odds Ratio 
#Need to put data into categories
# trying to get new columns for category variables (done)
attach(total)
total$Nugent.score.cat[Nugent.score > 6] <- "2" #positive
total$Nugent.score.cat[Nugent.score > 3 & Nugent.score <= 6] <- "1" #intermediate
total$Nugent.score.cat[Nugent.score <= 3] <- "0" #negative
detach(total)

#OR (but then need to sort into categories)
#only use this when column is a factor #see Ethnicity
total$Nugent.score.cat <- mapply(as.factor, total$Nugent.score)

#convert Nugent.score.cat from character into factor
total$Nugent.score.cat <- factor(total$Nugent.score.cat)

#apply this code to the rest of our variables

#Amsels
attach(total)
total$Amsels.cat[Amsels < 4] <- "0" #negative
total$Amsels.cat[Amsels = 4] <- "1" #positive
detach(total)
#convert Amsels.cat from character into factor
total$Amsels.cat <- factor(total$Amsels.cat)

#Age
attach(total)
total$Age.cat[Age < 20] <- "1" 
total$Age.cat[Age >= 20 & Age <=29] <- "2"
total$Age.cat[Age >= 30 & Age <=39] <- "3"
total$Age.cat[Age >= 40] <- "4" 
detach(total)

#convert Age.cat from character into factor
total$Age.cat <- factor(total$Age.cat)

#BMI
attach(total)
total$BMI.cat[BMI < 18.5] <- "1" #underweight
total$BMI.cat[BMI >= 18.5 & BMI <=24.9] <- "2" #normal weight
total$BMI.cat[BMI >= 25 & BMI <=29.9] <- "3" #overweight
total$BMI.cat[BMI >= 30] <- "4" #obesity
detach(total)

#convert BMI.cat from character into factor
total$BMI.cat <- factor(total$BMI.cat)

#Ethinicty
#needs to be character to reassign value
total$Ethnicity.cat <- mapply(as.character, total$Ethnicity)
#create categories
total$Ethnicity.cat[total$Ethnicity.cat=='Caucasian'] <- '1'
total$Ethnicity.cat[total$Ethnicity.cat=='Asian'] <- '2'
total$Ethnicity.cat[total$Ethnicity.cat=='Aboriginal'] <- '3'
total$Ethnicity.cat[total$Ethnicity.cat=='Other (Arab)'] <- '4'
total$Ethnicity.cat[total$Ethnicity.cat=='Other (Haida/Scottish)'] <- '4'

#convert Ethnicity.cat from character into factor
total$Ethnicity.cat <- factor(total$Ethnicity.cat)

#Martial Status
#needs to be character to reassign value
total$Marital.Status.cat <- mapply(as.character, total$Marital.Status)
#create categories
total$Marital.Status.cat[total$Marital.Status.cat=='Single'] <- '1'
total$Marital.Status.cat[total$Marital.Status.cat=='Married/Common Law'] <- '2'
total$Marital.Status.cat[total$Marital.Status.cat=='Other'] <- '3'
total$Marital.Status.cat[total$Marital.Status.cat=='Other (engaged)'] <- '3'

#convert Marital.Status.cat from character into factor
total$Marital.Status.cat <- factor(total$Marital.Status.cat)

#Highest Education level attained
#needs to be character to reassign value
total$Highest.Education.Level.cat <- mapply(as.character, total$Highest.Education.Level)
#create categories
total$Highest.Education.Level.cat[total$Highest.Education.Level.cat=='did not complete highschool'] <- '1'
total$Highest.Education.Level.cat[total$Highest.Education.Level.cat=='Highschool'] <- '2'
total$Highest.Education.Level.cat[total$Highest.Education.Level.cat=='some post-secondary'] <- '3'
total$Highest.Education.Level.cat[total$Highest.Education.Level.cat=='post-secondary'] <- '4'
total$Highest.Education.Level.cat[total$Highest.Education.Level.cat=='graduate degree'] <- '5'

#convert Highest.Education.Level.cat from character into factor
total$Highest.Education.Level.cat <- factor(total$Highest.Education.Level.cat)

#convert integers into factors
#Current or Chronic conditions
# yes-1, no-0
total$Current.or.chronic.conditions...y.1..n.0. <- factor(total$Current.or.chronic.conditions...y.1..n.0.)

#convert integers into factors
#Gential infection history
# yes-1, no-0
total$Genital.Infections..y.1..n.0. <- factor(total$Genital.Infections..y.1..n.0.)

#convert factor into numeric 
#BV episodes in the last 2 months
#convert to character so can set 'unsure' and 'chronic' values to NA
total$BV..number.of.episodes.2.months. <- mapply(as.character, total$BV..number.of.episodes.2.months.)
total$BV..number.of.episodes.2.months.[total$BV..number.of.episodes.2.months.=='chronic'] <- ''
total$BV..number.of.episodes.2.months.[total$BV..number.of.episodes.2.months.=='unsure'] <- ''
#character to numeric creates NA in blank spaces
total$BV..number.of.episodes.2.months. <- as.integer(total$BV..number.of.episodes.2.months.)

#BV episodes in the last year
total$BV..number.of.episodes.year. <- mapply(as.character, total$BV..number.of.episodes.year.)
total$BV..number.of.episodes.year.[total$BV..number.of.episodes.year.=='chronic'] <- ''
total$BV..number.of.episodes.year.[total$BV..number.of.episodes.year.=='unsure'] <- ''
#character to numeric creates NA in blank spaces
total$BV..number.of.episodes.year. <- as.integer(total$BV..number.of.episodes.year.)

#BV episodes in lifetime
total$BV..number.of.episodes.lifetime. <- mapply(as.character, total$BV..number.of.episodes.lifetime.)
total$BV..number.of.episodes.lifetime.[total$BV..number.of.episodes.lifetime.=='chronic'] <- ''
total$BV..number.of.episodes.lifetime.[total$BV..number.of.episodes.lifetime.=='unsure'] <- ''
#character to numeric creates NA in blank spaces
total$BV..number.of.episodes.lifetime. <- as.integer(total$BV..number.of.episodes.lifetime.)

#Yeast episodes in the last 2 months
total$Yeast..2months. <- mapply(as.character, total$Yeast..2months.)
total$Yeast..2months.[total$Yeast..2months.=='chronic'] <- ''
#character to numeric creates NA in blank spaces
total$Yeast..2months. <- as.integer(total$Yeast..2months.)

#Yeast episodes in the last year
total$Yeast..year. <- mapply(as.character, total$Yeast..year.)
total$Yeast..year.[total$Yeast..year.=='chronic'] <- ''
#character to numeric creates NA in blank spaces
total$Yeast..year. <- as.integer(total$Yeast..year.)

#Yeast episodes in lifetime
total$Yeast..lifetime. <- mapply(as.character, total$Yeast..lifetime.)
total$Yeast..lifetime.[total$Yeast..lifetime.=='chronic'] <- ''
total$Yeast..lifetime.[total$Yeast..lifetime.=='unk'] <- ''
#character to numeric creates NA in blank spaces
total$Yeast..lifetime. <- as.integer(total$Yeast..lifetime.)

#UTI episodes in the last 2 months
total$UTI..2.months. <- mapply(as.character, total$UTI..2.months.)
total$UTI..2.months.[total$UTI..2.months.=='yes'] <- ''
#character to numeric creates NA in blank spaces
total$UTI..2.months. <- as.integer(total$UTI..2.months.)

#Trich episodes in lifetime
total$Trich..lifetime. <- mapply(as.character, total$Trich..lifetime.)
total$Trich..lifetime.[total$Trich..lifetime.=='chronic'] <- ''
#character to numeric creates NA in blank spaces
total$Trich..lifetime. <- as.integer(total$Trich..lifetime.)

#convert integers into factors
#Antimicrobial use in the past 3 months
# yes-1, no-0
total$Antimicrobial.Use..y.1..n.0. <- factor(total$Antimicrobial.Use..y.1..n.0.)

#convert integers into factors
#(non)Prescription use in the past 2 months
# yes-1, no-0
total$X.Non..Prescription..y.1..n.0. <- factor(total$X.Non..Prescription..y.1..n.0.)

#Frequency of Menstrual Period
#needs to be character to reassign value
total$Freq.of.Menstrual.Period.cat <- mapply(as.character, total$Freq.of.Menstrual.Period)
#create categories
total$Freq.of.Menstrual.Period.cat[total$Freq.of.Menstrual.Period.cat=='Regular'] <- '1'
total$Freq.of.Menstrual.Period.cat[total$Freq.of.Menstrual.Period.cat=='Altered by contraception'] <- '2'
total$Freq.of.Menstrual.Period.cat[total$Freq.of.Menstrual.Period.cat=='No'] <- '3'

#convert Freq.of.Menstrual.Period.cat from character into factor
total$Freq.of.Menstrual.Period.cat <- factor(total$Freq.of.Menstrual.Period.cat)

#Frequency of Tampon Use
#needs to be character to reassign value
total$Tampon.Use.cat <- mapply(as.character, total$Tampon.Use)
#create categories
total$Tampon.Use.cat[total$Tampon.Use.cat=='never'] <- '1'
total$Tampon.Use.cat[total$Tampon.Use.cat=='sometimes but not for every period'] <- '2'
total$Tampon.Use.cat[total$Tampon.Use.cat=='every period, part of the time'] <- '3'
total$Tampon.Use.cat[total$Tampon.Use.cat=='every period, exclusively'] <- '4'

#convert Freq.of.Menstrual.Period.cat from character into factor
total$Tampon.Use.cat <- factor(total$Tampon.Use.cat)

#convert integers into factors
#Symptoms
# yes-1, no-0
total$X.Non..Prescription..y.1..n.0. <- factor(total$X.Non..Prescription..y.1..n.0.)

#data isn't accurate for symptoms so created new column to separate symptoms
#seen in past 2 weeks and past 48 hours and which symptoms
#add new column for presence of symptoms in 2 weeks
total[,"Presence.Symptoms.2wks"]  <- c(1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,1,0,1,
                                       1,1,1,1,1,1,1,1)
#convert numeric into factor
# yes-1, no-0
total$Presence.Symptoms.2wks <- factor(total$Presence.Symptoms.2wks)

#add new column for presence of abnormal discharge in 2 weeks
total[,"Abnormal.discharge.2wks"]  <- c(1,1,0,1,0,1,1,1,1,0,1,0,0,0,0,1,0,1,
                                        1,1,1,1,1,1,1,1)
#convert numeric into factor
# yes-1, no-0
total$Abnormal.discharge.2wks <- factor(total$Abnormal.discharge.2wks)

#add new column for presence of abnormal odor in 2 weeks
total[,"Abnormal.odor.2wks"]  <- c(0,1,0,0,0,0,0,0,1,0,1,0,1,0,0,1,0,0,0,1,
                                   1,0,0,0,0,1)
#convert numeric into factor
# yes-1, no-0
total$Abnormal.odor.2wks <- factor(total$Abnormal.odor.2wks)

#add new column for presence of irritation/discomfort in 2 weeks
total[,"Irritation.Discomfort.2wks"]  <- c(1,1,1,0,1,0,1,1,0,0,"",0,1,1,1,1,
                                           0,1,1,0,1,1,1,1,1,1)
#convert numeric into factor
# yes-1, no-0
total$Irritation.Discomfort.2wks <- factor(total$Irritation.Discomfort.2wks)

#add new column for presence of other symptoms in 2 weeks
total[,"Other.Symptoms.2wks"]  <- c(0,0,0,0,0,0,0,1,0,0,"",0,0,0,0,1,0,0,0,
                                    0,1,0,1,1,1,1)
#convert numeric into factor
# yes-1, no-0
total$Other.Symptoms.2wks <- factor(total$Other.Symptoms.2wks)

#add new column for presence of symptoms in 48 hrs
total[,"Presence.Symptoms.48hrs"]  <- c(1,1,1,0,1,1,1,1,1,0,"",1,0,1,1,1,0,
                                        1,1,0,1,1,1,1,1,1)
#convert numeric into factor
# yes-1, no-0
total$Presence.Symptoms.48hrs <- factor(total$Presence.Symptoms.48hrs)

#add new column for presence of abnormal discharge in 48 hrs
total[,"Abnormal.discharge.48hrs"]  <- c(1,1,0,0,0,1,1,1,1,0,"",0,0,0,0,1,0,
                                         1,1,"",1,1,1,1,1,1)
#convert numeric into factor
# yes-1, no-0
total$Abnormal.discharge.48hrs <- factor(total$Abnormal.discharge.48hrs)

#add new column for presence of abnormal odor in 48 hrs
total[,"Abnormal.odor.48hrs"]  <- c(0,1,0,0,0,0,0,0,1,0,"",0,0,0,0,1,0,0,0,
                                    "",1,0,0,1,0,1)
#convert numeric into factor
# yes-1, no-0
total$Abnormal.odor.48hrs <- factor(total$Abnormal.odor.48hrs)

#add new column for presence of irritation/discomfort in 48 hrs
total[,"Irritation.Discomfort.48hrs"]  <- c()
#convert numeric into factor
# yes-1, no-0
total$Irritation.Discomfort.48hrs <- factor(total$Irritation.Discomfort.48hrs)

#add new column for presence of other symptoms in 48 hrs
total[,"Other.Symptoms.48hrs"]  <- c()
#convert numeric into factor
# yes-1, no-0
total$Other.Symptoms.48hrs <- factor(total$Other.Symptoms.48hrs)