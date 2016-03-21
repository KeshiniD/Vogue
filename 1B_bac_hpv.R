#relating Vogue 1B HPV data to bacterial data

# load datasets
bac <- read.csv(file = "Vogue1B_bac.csv")
hpv <- read.csv(file= "HPVinHIV_types.csv")

#load packages
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)

#select participants with HPV data available (omit those excluded from 1B)
bac <- read.csv(file = "Vogue1B_bac.csv")
hpv <- read.csv(file= "HPVinHIV_types.csv")
nums <- substring(hpv$Vogue.1B.ID, 4)
ids <- paste0("Vogue.1B.01.", nums)
bac2 <- bac[, which(colnames(bac) %in% c(ids, "X"))]

### change study ID
hpv$Vogue.1B.ID <-
  paste0("Vogue.1B.01.",
         substring(hpv$Vogue.1B.ID, nchar("01-")+1))

#invert table and merge datasets together


