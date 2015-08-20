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

#adjusted odds ratio example
install.packages("PredictABEL", dependencies = TRUE)
library(PredictABEL)
data(ExampleData)
# specify column number of outcome variable
cOutcome <- 2
# specify column numbers of non-genetic predictors
cNonGenPred <- c(3:10)
# specify column numbers of non-genetic predictors that are categorical
cNonGenPredCat <- c(6:8)
# specify column numbers of genetic predictors
cGenPred <- c(11,13:16)
# specify column numbers of genetic predictors that are categorical
cGenPredCat <- c(0)
riskmodel <- fitLogRegModel(data=total, cOutcome=cOutcome,
                            cNonGenPreds=cNonGenPred, cNonGenPredsCat=cNonGenPredCat,
                            cGenPreds=cGenPred, cGenPredsCat=cGenPredCat)
#categorize variables (ie 19-25 = 1 etc.)
# obtain multivariate OR(95% CI) for all predictors of the fitted model
a <- ORmultivariate(riskModel=riskmodel, filename="multiOR.txt")
a <- as.data.frame(a)

#apply to own data
# specify column number of outcome variable
cOutcome <- 2
# specify column numbers of non-genetic predictors
cNonGenPred <- c(4,5,38, 39:43,68:94)
# specify column numbers of non-genetic predictors that are categorical
cNonGenPredCat <- c(9,10)
# specify column numbers of genetic predictors
cGenPred <- c(0)
# specify column numbers of genetic predictors that are categorical
cGenPredCat <- c(0)

#Odss Ratio figured out. Need to put data into categories
#data has to be in factor form
total$Symptoms..y.1..n.0. <- factor(total$Symptoms..y.1..n.0.)
total$abnormal.discharge..y.1..n.0. <- factor(total$abnormal.discharge..y.1..n.0.)
total$Nugent.score <- factor(total$Nugent.score)

#odds ratio code
mylogit <- glm(formula = Nugent.score ~ Symptoms..y.1..n.0. + 
                 abnormal.discharge..y.1..n.0., data = total, 
               family = binomial(link = "logit"))
mylogit <- glm(formula = Nugent.score ~ Symptoms..y.1..n.0. + 
                 abnormal.discharge..y.1..n.0., data = total, 
               family = binomial)

mylogit
confint(mylogit) #CI intervals
exp(cbind(OR = coef(mylogit), confint(mylogit))) #ORs and CIs
exp(coef(mylogit)) #only ORs
summary(mylogit)# for really nice table
#cannot convert glm into data.frame
