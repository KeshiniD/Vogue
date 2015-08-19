#odds ratio
## needs epitools package
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

total <- read.csv(file.path("1B2metabac.csv"))

tapw <- c("Lowest", "Intermediate", "Highest") #example
outc <- c("Case", "Control")
dat <- matrix(c(2, 29, 35, 64, 12, 6),3,2,byrow=TRUE)
dimnames(dat) <- list("Tap water exposure" = tapw, "Outcome" = outc)
oddsratio(dat, rev="c")
oddsratio.midp(dat, rev="c")
oddsratio.fisher(dat, rev="c")
oddsratio.wald(dat, rev="c")
oddsratio.small(dat, rev="c")

# the zeros are probably a problem
t <- c("lacto", "no lacto")
o <- c("no", "inter", "yes")
dat <- matrix(c(64, 1601, 69, 0, 0, 0),2,3,byrow=TRUE)
dimnames(dat) <- list("Lacto presence" = t, "Outcome" = o)
oddsratio(dat, rev="c")

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
riskmodel <- fitLogRegModel(data=ExampleData, cOutcome=cOutcome,
                            cNonGenPreds=cNonGenPred, cNonGenPredsCat=cNonGenPredCat,
                            cGenPreds=cGenPred, cGenPredsCat=cGenPredCat)
#categorize variables (ie 19-25 = 1 etc.)
# obtain multivariate OR(95% CI) for all predictors of the fitted model
ORmultivariate(riskModel=riskmodel, filename="multiOR.txt")