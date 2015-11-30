#load packages
library(plyr)
library(dplyr)
library(tidyr)

#load dataset
data <- read.csv(file.path("1A.csv"))



#selecting 104/310; all participants
#select only Study.ID column
data2 <- data %>%
  select(Study.ID)

#transpose data in order for function to select participants
x <- as.data.frame(t(data2))

#random selection of participants
a <- sample(x, size = 104, replace = FALSE, prob = NULL)
a <- as.data.frame(t(a))



#selecting 104/156; participants only in CST I
#select only Study.ID column
data2 <- data[ which(data$CST=='I'), ]
#select only Study.ID column
data2 <- data2 %>%
  select(Study.ID)

#transpose data in order for function to select participants
x <- as.data.frame(t(data2))

#random selection of participants
a <- sample(x, size = 104, replace = FALSE, prob = NULL)
a <- as.data.frame(t(a))



#selecting 104/135; participants only in CST I and without Antimicrobial use
data2 <- data[ which(data$CST=='I' 
                     & data$Antimicrobials < 1), ]
#select only Study.ID column
data2 <- data2 %>%
  select(Study.ID)

#transpose data in order for function to select participants
x <- as.data.frame(t(data2))

#random selection of participants
a <- sample(x, size = 104, replace = FALSE, prob = NULL)
a <- as.data.frame(t(a))
