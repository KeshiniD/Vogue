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
#na.omit removes NAs from being plotted
ggplot(na.omit(data2), aes(x = Participants, y = Mollicutes, 
                        fill = Ureaplasma)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  ylab("Mollicutes") +
    ggtitle("Mollicutes and Ureaplasma of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis")

#facet_wrap mollicutes, and show ureaplasma in each cat.
ggplot(na.omit(data2), aes(x = factor(1), y = Mollicutes.counts,
                        fill = Ureaplasma)) + 
  geom_bar(stat = "identity", width = 1) + xlab("Mollicutes") + 
  ylab("Number of Participants") +
  ggtitle("Mollicutes and Ureaplasma of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis") + 
  facet_wrap(~Mollicutes)

#pie chart 
ggplot(na.omit(data3), aes(x = factor(1), y = Mollicutes.percentage, 
                         fill = Ureaplasma)) + 
  geom_bar(stat = "identity", width = 1) + xlab("") + 
  ylab("Mollicutes") +
  ggtitle("Mollicutes and Ureaplasma of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis") + 
  facet_wrap(~Mollicutes) + coord_polar(theta="y") 

#need it to be 100%, going to make new column for proportion
data3 <- tbl_df(data2) %>% 
  group_by(Mollicutes) %>%
  select(Participants, Mollicutes, Mollicutes.counts, Ureaplasma) %>%
  mutate(Mollicutes.percentage = Mollicutes.counts/(sum(Mollicutes.counts))*100) %>% # can either have % or decimal
  arrange(Ureaplasma) #so species are together and not separated
#use data3, and mollicute.percentage above

#plot per CST
#call for entire 1B2 data
total <- read.csv(file.path("1B2metabac_condensed.csv"))
#remove random columns
total$X  <-  NULL
total$X.1  <-  NULL
total$X.2  <-  NULL

#merge datasets together (data3, and total)
data4 <- join(data3, total, type="full") #make sure both vectors are dataframes

#select variables wish to plot
data5 <- data4 %>%
  select (Participants, Mollicutes, Mollicutes.counts, 
          Mollicutes.percentage, Ureaplasma, CST) %>%
  arrange(Ureaplasma) #so species are together

#plot
#facet_wrap mollicutes + CST, and show ureaplasma in each cat.
ggplot(na.omit(data5), aes(x = factor(1), y = Mollicutes.counts,
                           fill = Ureaplasma)) + 
  geom_bar(stat = "identity", width = 1) + xlab("CSTs") + 
  ylab("Number of Participants") +
  ggtitle("Mollicutes and Ureaplasma of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis") + 
  facet_wrap(~Mollicutes + CST)

#just ureaplasma
ggplot(na.omit(data5), aes(x = factor(1), y = Mollicutes.counts,
                           fill = Ureaplasma)) + 
  geom_bar(stat = "identity", width = 1) + xlab("CSTs") + 
  ylab("Number of Participants") +
  ggtitle("Ureaplasma of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis") + 
  facet_wrap(~CST)

#just mollicutes
data6 <- data4 %>%
  select (Participants, Mollicutes, Mollicutes.counts, 
          Mollicutes.percentage, Ureaplasma, CST) %>%
  arrange(Mollicutes) #so mollicutes together

#plot
ggplot(na.omit(data6), aes(x = factor(1), y = Mollicutes.counts,
                           fill = Mollicutes)) + 
  geom_bar(stat = "identity", width = 1) + xlab("CSTs") + 
  ylab("Number of Participants") +
  ggtitle("Mollicutes of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis") + 
  facet_wrap(~CST)

#pie chart 
ggplot(na.omit(data7), aes(x = factor(1), y = Mollicutes.percentage, 
                           fill = Ureaplasma)) + 
  geom_bar(stat = "identity", width = 1) + xlab("") + 
  ylab("CSTs") +
  ggtitle("Mollicutes and Ureaplasma of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis") + 
  facet_wrap(~Mollicutes + CST) + coord_polar(theta="y") 

#just ureaplasma
my_title <- expression(paste(italic("Ureaplasma"), " species of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis"))

ggplot(na.omit(data8), aes(x = factor(1), y = Mollicutes.percentage, 
                           fill = Ureaplasma)) + 
  geom_bar(stat = "identity", width = 1) + xlab("") + 
  ylab("Community State Types") +
  ggtitle(my_title) + 
  facet_wrap(~CST) + coord_polar(theta="y")   + 
  theme(legend.position = "bottom", legend.key.size = unit(.35, "cm"), 
        legend.title = element_blank(), plot.title = element_text(size=18), 
        axis.text = element_text(size=12), axis.title = element_text(size=16), 
        legend.text = element_text(size=14)) 

#just mollicutes #not complete circles
ggplot(na.omit(data8), aes(x = factor(1), y = Mollicutes.percentage, 
                           fill = Mollicutes)) + 
  geom_bar(stat = "identity", width = 1) + xlab("") + 
  ylab("Community State Types") +
  ggtitle("Mollicutes of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis") + 
  facet_wrap(~CST) + coord_polar(theta="y")  + 
  theme(legend.position = "bottom", legend.key.size = unit(.35, "cm"), 
        legend.title = element_blank(), plot.title = element_text(size=18), 
        axis.text = element_text(size=12), axis.title = element_text(size=16), 
        legend.text = element_text(size=14)) 

#make circles complete by creating new mollicute.percentages
#mollicute+ureaplasma
#need it to be 100%, going to make new column for proportion
data7 <- tbl_df(data5) %>% 
  group_by(CST, Mollicutes) %>%
  select(Participants, Mollicutes, Mollicutes.counts, Ureaplasma) %>%
  mutate(Mollicutes.percentage = Mollicutes.counts/(sum(Mollicutes.counts))*100) %>% # can either have % or decimal
  arrange(CST) 
#use data7, and mollicute.percentage above in mollicutes+ureaplasma plot

#just ureaplasma
data8 <- tbl_df(data5) %>% 
  group_by(CST, Mollicutes.counts) %>%
  select(Participants, Mollicutes, Mollicutes.counts, Ureaplasma, CST) %>%
  mutate(Mollicutes.percentage = Mollicutes.counts/(sum(Mollicutes.counts))*100) %>% # can either have % or decimal
  arrange(CST) 
#use data8 and mollicute.percentages above in just ureaplasma plot
#and in just mollicutes plot