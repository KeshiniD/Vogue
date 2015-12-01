#load packages
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

#load dataset
data <- read.csv(file.path("1B2_Mollicute_Ureaplasma.csv"))


#rename variables
#data <- dplyr::rename(data, '16s_copies_per_swab' = X16s_copies_per_swab, 
#                      '16s_category' = X16s_category)
#error because cannot start with a number, unless put in quotes"
#still presents problems when try to call for variable


#make new cat which indicate positive and negative, and dif ureaplasma
#Mollicutes
#rename
data <- dplyr::rename(data, Mollicutes.cat = mollicutes, 
                      Ureaplasma.cat = ureaplasma)

#new mollicute.cat
data$Mollicutes[data$Mollicutes.cat <= 1 ] <- "Positive" 
data$Mollicutes[data$Mollicutes.cat >= 2] <- "Negative" 

#convert Mollicutes from character into factor
data$Mollicutes <- factor(data$Mollicutes)

#new ureaplasma.cat
data$Ureaplasma[data$Ureaplasma.cat <= 1 ] <- "Negative" 
data$Ureaplasma[data$Ureaplasma.cat <= 2 & 
                  data$Ureaplasma.cat >= 2] <- "Parvum" 
data$Ureaplasma[data$Ureaplasma.cat >= 3] <- "Urealyticum" 

#convert Ureaplasma from character into factor
data$Ureaplasma <- factor(data$Ureaplasma)

#make variables into factors
data$Mollicutes.cat <- factor(data$Mollicutes.cat)
data$Ureaplasma.cat <- factor(data$Ureaplasma.cat)
data$X16s_category <- factor(data$X16s_category)

#summary of each variables
summary(data$mollicutes)
summary(data$ureaplasma)
summary(data$X16s_category)

#Mollicutes 
#1-positive: 18
#2-negative: 6
#3-not tested: 2

#Ureaplasma
#1-negative: 5
#2-parvum: 14
#3-urealyticum: 5
#4-parvum&urealyticum: 0
#5-not tested: 2

#16s cat
#1-<10˄: 0
#2-10˄4: 0
#3-10˄5: 0
#4-10˄6: 0
#5-10˄7: 4
#6-10˄8: 9
#7-10˄9: 10
#8->10˄9: 1
#NAs: 2

#plots so far; same species are not together
#need to order ureaplasma to solve this
data2 <- data %>%
  arrange(Ureaplasma)
#use data2 below

#also need number of individuals on y-axis
#create new cat.for that
#new mollicute.cat
data2$Mollicutes.counts[data2$Mollicutes.cat >= 1 ] <- "1" 
#make into integer
data2$Mollicutes.counts <- as.integer(data2$Mollicutes.counts)

#plot mollicute and ureaplasma for participants
ggplot(data = data2, aes(x = Participants, y = Mollicutes, 
                        fill = Ureaplasma)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  ylab("Mollicutes") +
    ggtitle("Mollicutes and Ureaplasma of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis")
#drop NAs from plot

#facet_wrap mollicutes, and show ureaplasma in each cat.
ggplot(data = data2, aes(x = factor(1), y = Mollicutes.counts, 
                        fill = Ureaplasma)) + 
  geom_bar(stat = "identity", width = 1) + xlab("Mollicutes") + 
  ylab("Number of Participants") +
  ggtitle("Cpn60 Species Characterization of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis") + 
  facet_wrap(~Mollicutes)

#pie chart #not complete circle
ggplot(data = data2, aes(x = factor(1), y = Mollicutes.counts, 
                         fill = Ureaplasma)) + 
  geom_bar(stat = "identity", width = 1) + xlab("Mollicutes") + 
  ylab("Number of Participants") +
  ggtitle("Cpn60 Species Characterization of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis") + 
  facet_wrap(~Mollicutes) + coord_polar(theta="y") 
