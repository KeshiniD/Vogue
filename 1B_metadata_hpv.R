#load packages
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)

#load datasets
hiv <- read.csv(file="Vogue1B_HIVdata.csv")
hpv <- read.csv(file= "HPVinHIV_types.csv")
data <- read.csv(file="Vogue1B_collection2.csv")

#select participants with HPV data available (omit those excluded from 1B)
hiv <- read.csv(file="Vogue1B_HIVdata.csv")
hpv <- read.csv(file= "HPVinHIV_types.csv")
nums <- substring(hpv$Vogue.1B.ID, 4)
ids <- paste0("Vogue 1B 01-", nums)
hiv2 <- hiv[which(hiv$Study.ID %in% ids), ]

data <- read.csv(file="Vogue1B_collection2.csv")
hpv <- read.csv(file= "HPVinHIV_types.csv")
nums <- substring(hpv$Vogue.1B.ID, 4)
ids <- paste0("Vogue 1B 01-", nums)
data2 <- data[which(data$study_id %in% ids), ]

### change study ID
hpv$Vogue.1B.ID <-
  paste0("Vogue 1B 01-",
         substring(hpv$Vogue.1B.ID, nchar("01-")+1))

#select hpv types, and merge datasets together
hpv2 <- hpv %>%
  select (Vogue.1B.ID, t06, t11, t16, t18, t26, t31, t33, t34, t35, t39, t40, 
          t42, t44, t45, t51, t52, t53, t54, t56, t58, t59, t61, t62, t66, t67, 
          t68, t69, t70, t71, t72, t73, t81, t82, t83, t84, t89, Number.of.Different.HPV.Types) 

#merge all datasets
data2 <- dplyr::rename(data2, Study.ID = study_id) #same column name
hpv3 <- dplyr::rename(hpv2, Study.ID = Vogue.1B.ID) #same column name
total<-join(data2, hiv2, type="full") #merge
total2 <- join(total, hpv3, type='full')

#remove participants 55, 57; excluded from 1B
#last two rows
total3 <- total2[c(-31,-32),]

#add CSTs
CST <- read.csv(file="Vogue1B_CST.csv")

### change study ID
total3$Study.ID <-
  paste0("Vogue1B.01.",
         substring(total3$Study.ID, nchar("Vogue 1B 01-")+1))

CST$Participants <-
  paste0("Vogue1B.01.0",
         substring(CST$Participants, nchar("Vogue1B.1.")+1))
#now they are the same

#select columns want
CST2 <- CST %>%
  select (Participants, CST)

#remove empty rows
CST3 <- CST2[c(-55:-69),]

#select participants which have hpv types
#CST3
#total3
nums <- substring(total3$Study.ID, 12)
ids <- paste0("Vogue1B.01.", nums)
CST4 <- CST3[which(CST3$Participants %in% ids), ]

#rename participant column and merge
CST5 <- dplyr::rename(CST4, Study.ID = Participants) #same column name
total4<-join(total3, CST5, type="full") #merge

#write to file
#write.csv(total4, "1B_full.csv")

#clean up variables, group
total <- read.csv(file="1B_full.csv")
