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

###################
#Virome 1A Participants
#load packages
library(plyr)
library(dplyr)
library(tidyr)

#load dataset
data <- read.csv(file.path("1A_virome.csv"))

###################
#selecting 3-CST I participants
#select only Study.ID column
data2 <- data[ which(data$CST=='I'), ]
data2 <- data2 %>%
  select(study_id)

#transpose data in order for function to select participants
x <- as.data.frame(t(data2))

#random selection of participants
a <- sample(x, size = 3, replace = FALSE, prob = NULL)
a <- as.data.frame(t(a))

#####################
#selecting 3-CST III participants
#select only Study.ID column
data2 <- data[ which(data$CST=='III'), ]
data2 <- data2 %>%
  select(study_id)

#transpose data in order for function to select participants
x <- as.data.frame(t(data2))

#random selection of participants
b <- sample(x, size = 3, replace = FALSE, prob = NULL)
b <- as.data.frame(t(b))

#################
#selecting 3-CST IVA participants
#select only Study.ID column
data2 <- data[ which(data$CST=='IVA'), ]
data2 <- data2 %>%
  select(study_id)

#transpose data in order for function to select participants
x <- as.data.frame(t(data2))

#random selection of participants
c <- sample(x, size = 3, replace = FALSE, prob = NULL)
c <- as.data.frame(t(c))

#################
#selecting 3-CST IVC participants
#select only Study.ID column
data2 <- data[ which(data$CST=='IVC'), ]
data2 <- data2 %>%
  select(study_id)

#transpose data in order for function to select participants
x <- as.data.frame(t(data2))

#random selection of participants
d <- sample(x, size = 3, replace = FALSE, prob = NULL)
d <- as.data.frame(t(d))

#################
#only have 3-IVD anyways

#################
#selecting 6-CST V participants
#select only Study.ID column
data2 <- data[ which(data$CST=='V'), ]
data2 <- data2 %>%
  select(study_id)

#transpose data in order for function to select participants
x <- as.data.frame(t(data2))

#random selection of participants
e <- sample(x, size = 6, replace = FALSE, prob = NULL)
e <- as.data.frame(t(e))

#merge together
zz<-join(a, b, type="full")
zz<-join(zz, c, type="full")
zz<-join(zz, d, type="full")
zz<-join(zz, e, type="full")

#IVA and IVC includes individuals who switched
#save to file
#write this data to file
write.table(zz, "1A_virome_randomized_selection.csv", sep = ",", row.names = FALSE, quote = FALSE)
