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

#convert Ethnicity.cat from character into factor
total$Highest.Education.Level.cat <- factor(total$Highest.Education.Level.cat)
