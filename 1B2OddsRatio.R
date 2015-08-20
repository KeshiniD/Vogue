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
total <- read.csv(file.path("1B2metabac.csv"))

#Odss Ratio figured out. Need to put data into categories
#data has to be in factor form
total$Symptoms..y.1..n.0. <- factor(total$Symptoms..y.1..n.0.)
total$abnormal.discharge..y.1..n.0. <- factor(total$abnormal.discharge..y.1..n.0.)
total$Nugent.score <- factor(total$Nugent.score)

#odds ratio code
mylogit <- glm(formula = Nugent.score ~ Symptoms..y.1..n.0. + 
                 abnormal.discharge..y.1..n.0., Number.partners.in.past.year, data = total, 
               family = binomial(link = "logit"))
mylogit <- glm(formula = Nugent.score ~ Symptoms..y.1..n.0. + 
                 abnormal.discharge..y.1..n.0., data = total, 
               family = binomial) #gives same results thus far
#na in data is ok

mylogit
confint(mylogit) #CI intervals
exp(cbind(OR = coef(mylogit), confint(mylogit))) #ORs and CIs
exp(coef(mylogit)) #only ORs
summary(mylogit)# for really nice table
#cannot convert glm into data.frame

#above is binary data, below may be able to deal with non-binary data

#odds ratio
## needs epitools package
tapw <- c("Lowest", "Intermediate", "Highest") #example
outc <- c("Case", "Control")
dat <- matrix(c(2, 29, 35, 64, 12, 6),3,2,byrow=TRUE)
dimnames(dat) <- list("Tap water exposure" = tapw, "Outcome" = outc)
oddsratio(dat, rev="c")
oddsratio.midp(dat, rev="c")
oddsratio.fisher(dat, rev="c")
oddsratio.wald(dat, rev="c")
oddsratio.small(dat, rev="c")
riskratio(dat)

# the zeros are probably a problem
t <- c("lacto", "no lacto")
o <- c("no", "inter", "yes")
dat <- matrix(c(64, 1601, 69, 1, 2, 3),2,3,byrow=TRUE)
dimnames(dat) <- list("Lacto presence" = t, "Outcome" = o)
oddsratio(dat, rev="c")
riskratio(dat)

attach(total)
#can just give names, since used attach()
mytable <- table(total$Ethnicity,total$Martial.Status) #do it this way, and lengths dif
mytable


#reassign to binary codes
total2 <- model.matrix(~Ethnicity -1 , data=total)
total3 <- model.matrix(~Other.Bacteria -1 , data=total) 
#doesn't work with bacteria
#might be away to look at things that cannot be set into single column binary


total$Sexual.Partners[total$Sexual.Partners=='Male']<-'m'
#make numbers into categories
total$Nugent.score[Nugent.score > 6] <- "0"
total$Nugent.score[Nugent.score > 3 & Nugent.score <= 6] <- "1"
total$Nugent.score[Nugent.score <= 3] <- "2"
total <- rename(total$Sexual.Partners, c("Male" = "0", "Female" = "1"))
total$Sexual.Partners <- as.character(total$Sexual.Partners)
