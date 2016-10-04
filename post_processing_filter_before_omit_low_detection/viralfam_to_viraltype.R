#redone for filtered files; Sept28-16
#load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)

#call data and recode for DNa, RNA and phage
everyone <- read.csv("DNA_RNA_phage_viral_family_all_v2.csv", stringsAsFactors = FALSE)
vogueA <- read.csv("DNA_RNA_phage_viral_family_1A_v2.csv", stringsAsFactors = FALSE)
vogueB <- read.csv("DNA_RNA_phage_viral_family_1B_v2.csv", stringsAsFactors = FALSE)
vogue1b2 <- read.csv("DNA_RNA_phage_viral_family_1B2_v2.csv", stringsAsFactors = FALSE)

#omit empty column
everyone$X <- NULL
vogueA$X <- NULL
vogueB$X <- NULL
vogue1b2$X <- NULL

#ref manual
vref <- read.csv("recode_family_to_type.csv", header=FALSE, stringsAsFactors = FALSE)

#omit empty rows and columns
# vref <- vref %>%
#   select(V1, V2)
# vref <- vref[c(1:72),]

#recode families to virus types
#recode function
recoderFunc <- function(data, oldvalue, newvalue) {
  
  # convert any factors to characters
  
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  
  # create the return vector
  
  newvec <- data
  
  # put recoded values into the correct position in the return vector
  
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  
  newvec
  
}

#recode dataframes
#everyone
everyone2 <- recoderFunc(everyone, vref$V1, vref$V2)
vogueA2 <- recoderFunc(vogueA, vref$V1, vref$V2)
vogueB2 <- recoderFunc(vogueB, vref$V1, vref$V2)
vogue1b2a <- recoderFunc(vogue1b2, vref$V1, vref$V2)

#rename column
everyone2 <- dplyr::rename(everyone2, Virus_Type = Viral_Family)
vogueA2 <- dplyr::rename(vogueA2, Virus_Type = Viral_Family)
vogueB2 <- dplyr::rename(vogueB2, Virus_Type = Viral_Family)
vogue1b2a <- dplyr::rename(vogue1b2a, Virus_Type = Viral_Family)

#gathering like types                    
everyone3 <- ddply(everyone2,c("Virus_Type"),numcolwise(sum)) #includes all columns
vogueA3 <- ddply(vogueA2,c("Virus_Type"),numcolwise(sum)) #includes all columns
vogueB3 <- ddply(vogueB2,c("Virus_Type"),numcolwise(sum)) #includes all columns
vogue1b2b <- ddply(vogue1b2a,c("Virus_Type"),numcolwise(sum)) #includes all columns

#na to zero
everyone3[is.na(everyone3)] <- 0
vogueA3[is.na(vogueA3)] <- 0
vogueB3[is.na(vogueB3)] <- 0
vogue1b2b[is.na(vogue1b2b)] <- 0

#write to file
# write.csv(everyone3, "virus_types_all.csv")
# write.csv(vogueA3, "virus_types_1A.csv")
# write.csv(vogueB3, "virus_types_1B.csv")
# write.csv(vogue1b2b, "virus_types_1B2.csv")

#plot
#call data
everyone <- read.csv("virus_types_all.csv")
vogueA <- read.csv("virus_types_1A.csv")
vogueB <- read.csv("virus_types_1B.csv")
vogue1b2 <- read.csv("virus_types_1B2.csv")

everyone$X <- NULL

rownames(everyone) <- everyone[,1]
everyone[,1] <- NULL
everyone2 <- as.data.frame(t(everyone))
everyone2[is.na(everyone2)] <- 0
everyone2 <- add_rownames(everyone2, "Participants")

#bac counts
data <-
  gather(everyone2, key = 'Viral_Groups', value = 'Counts', Viral_DNA, Viral_RNA, 
         Phage, dsDNA_RT, ssRNA_RT, unclassified_viruses)

vmb <- tbl_df(data) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Groups, Counts) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Groups) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viral_Groups']]

#Dean added to order by factor levels
vmb$Viral_Groups <- factor(vmb$Viral_Groups, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('purple', 'green', 'blue', 'orange', 'red', 'yellow')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") +ggtitle("Metagenomic Characterization of the Vaginal Virome") + 
  theme(axis.title.x = element_text(size=30), axis.title.y =element_text(size=30), 
        plot.title =element_text(size=40), legend.background = element_rect(size=200)) 
########################################################################################
#plot freq
#1A
vogueA <- read.csv("virus_types_1A.csv")
vogueA$X <- NULL
rownames(vogueA) <- vogueA[,1] #have to make sure dataframe is all numeric for rowsums
vogueA[,1] <- NULL
vogueA2 <- vogueA %>%
  mutate(freq = rowSums(vogueA)) #calculating total sum for each viral type
vogueA <- read.csv("virus_types_1A.csv")
vogueA$X <- NULL
vogueA <- join(vogueA, vogueA2, type="full") #merge with orginal dataframe to keep freq & type col.

vogueA <- dplyr::rename(vogueA, Healthy_Asymptomatic = freq)

vogueA <- vogueA %>%
  select(Virus_Type, Healthy_Asymptomatic)

rownames(vogueA) <- vogueA[,1]
vogueA[,1] <- NULL
vogueA2 <- as.data.frame(t(vogueA))
vogueA2[is.na(vogueA2)] <- 0
vogueA2 <- add_rownames(vogueA2, "Cohort")

#1B
vogueB <- read.csv("virus_types_1B.csv")
vogueB$X <- NULL
rownames(vogueB) <- vogueB[,1]
vogueB[,1] <- NULL
vogueB2 <- vogueB %>%
  mutate(freq = rowSums(vogueB))
vogueB <- read.csv("virus_types_1B.csv")
vogueB$X <- NULL
vogueB <- join(vogueB, vogueB2, type="full")

vogueB <- dplyr::rename(vogueB, HIV_Positive = freq)

vogueB <- vogueB %>%
  select(Virus_Type, HIV_Positive)

rownames(vogueB) <- vogueB[,1]
vogueB[,1] <- NULL
vogueB2 <- as.data.frame(t(vogueB))
vogueB2[is.na(vogueB2)] <- 0
vogueB2 <- add_rownames(vogueB2, "Cohort")

#1B2
vogue1B2 <- read.csv("virus_types_1B2.csv")
vogue1B2$X <- NULL
rownames(vogue1B2) <- vogue1B2[,1]
vogue1B2[,1] <- NULL
vogue1B2b <- vogue1B2 %>%
  mutate(freq = rowSums(vogue1B2))
vogue1B2 <- read.csv("virus_types_1B2.csv")
vogue1B2$X <- NULL
vogue1B2 <- join(vogue1B2, vogue1B2b, type="full")

vogue1B2 <- dplyr::rename(vogue1B2, Recurrent_BV = freq)

vogue1B2 <- vogue1B2 %>%
  select(Virus_Type, Recurrent_BV)

rownames(vogue1B2) <- vogue1B2[,1]
vogue1B2[,1] <- NULL
vogue1B2a <- as.data.frame(t(vogue1B2))
vogue1B2a[is.na(vogue1B2a)] <- 0
vogue1B2a <- add_rownames(vogue1B2a, "Cohort")

#merge three cohorts together
total <- merge(vogueA2, vogueB2, all.x = TRUE, all.y = TRUE)
total <- merge(total, vogue1B2a, all.x = TRUE, all.y = TRUE)

total[is.na(total)] <- 0

#bac counts
data <-
  gather(total, key = 'Viral_Groups', value = 'Counts', Viral_DNA, Viral_RNA, 
         Phage, dsDNA_RT, ssRNA_RT, unclassified_viruses)

vmb <- tbl_df(data) %>% # finally got the percentages correct
  group_by(Cohort) %>%
  select(Cohort, Viral_Groups, Counts) %>%
  mutate(Relative.Abundance = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Cohort)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Groups) %>% summarize(count = mean(Relative.Abundance)) %>% arrange(desc(count)) %>% .[['Viral_Groups']]

#Dean added to order by factor levels
vmb$Viral_Groups <- factor(vmb$Viral_Groups, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('purple', 'green', 'red', 'blue', 'orange', 'yellow')

ggplot(data = vmb, aes(x = Cohort, y = Relative.Abundance, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) + 
  ggtitle("Metagenomic Characterization of the Vaginal Virome") + ylab("Relative Abundance (%)") +
  theme(axis.title.x = element_text(size=20), axis.title.y =element_text(size=20), 
        plot.title =element_text(size=25), legend.background = element_rect(size=100), 
        axis.text.y = element_text(size=15)) 

######################################################################################
#plot
#call data
vogueA <- read.csv("virus_types_1A.csv")
vogueB <- read.csv("virus_types_1B.csv")
vogue1b2 <- read.csv("virus_types_1B2.csv")

vogueA$X <- NULL
vogueA$freq <- NULL

rownames(vogueA) <- vogueA[,1]
vogueA[,1] <- NULL
vogueA <- as.data.frame(t(vogueA))
vogueA[is.na(vogueA)] <- 0
vogueA <- add_rownames(vogueA, "Participants")

#bac counts
data <-
  gather(vogueA, key = 'Viral_Groups', value = 'Counts', Viral_DNA, Viral_RNA, 
         Phage, dsDNA_RT, ssRNA_RT, unclassified_viruses)

vmb <- tbl_df(data) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Groups, Counts) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Groups) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viral_Groups']]

#Dean added to order by factor levels
vmb$Viral_Groups <- factor(vmb$Viral_Groups, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('purple', 'red', 'blue', 'orange', 'green', 'yellow')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") + 
  ggtitle("Metagenomic Characterization of the Vaginal Virome of Healthy-Asymptomatic Women") +
  theme(axis.title.x = element_text(size=25), axis.title.y =element_text(size=25), 
        plot.title =element_text(size=20), legend.background = element_rect(size=200)) 

#########################################################################################
vogueB <- read.csv("virus_types_1B.csv")
vogue1b2 <- read.csv("virus_types_1B2.csv")

vogueB$X <- NULL
vogueB$freq <- NULL

rownames(vogueB) <- vogueB[,1]
vogueB[,1] <- NULL
vogueB <- as.data.frame(t(vogueB))
vogueB[is.na(vogueB)] <- 0
vogueB <- add_rownames(vogueB, "Participants")

#bac counts
data <-
  gather(vogueB, key = 'Viral_Groups', value = 'Counts', Viral_DNA, Viral_RNA, 
         Phage, dsDNA_RT, ssRNA_RT, unclassified_viruses)

vmb <- tbl_df(data) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Groups, Counts) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Groups) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viral_Groups']]

#Dean added to order by factor levels
vmb$Viral_Groups <- factor(vmb$Viral_Groups, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('purple', 'green', 'red', 'orange', 'blue', 'yellow')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") + 
  ggtitle("Metagenomic Characterization of the Vaginal Virome of HIV Positive Women") +
  theme(axis.title.x = element_text(size=20), axis.title.y =element_text(size=20), 
        plot.title =element_text(size=25), legend.background = element_rect(size=200)) 

###########################################################################################
vogue1b2 <- read.csv("virus_types_1B2.csv")

vogue1b2$X <- NULL
vogue1b2$freq <- NULL

rownames(vogue1b2) <- vogue1b2[,1]
vogue1b2[,1] <- NULL
vogue1b2 <- as.data.frame(t(vogue1b2))
vogue1b2[is.na(vogue1b2)] <- 0
vogue1b2 <- add_rownames(vogue1b2, "Participants")

#bac counts
data <-
  gather(vogue1b2, key = 'Viral_Groups', value = 'Counts', Viral_DNA, Viral_RNA, 
         Phage, ssRNA_RT, unclassified_viruses)

vmb <- tbl_df(data) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Groups, Counts) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Groups) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viral_Groups']]

#Dean added to order by factor levels
vmb$Viral_Groups <- factor(vmb$Viral_Groups, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('purple', 'green', 'red', 'blue', 'orange')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") + 
  ggtitle("Metagenomic Characterization of the Vaginal Virome of Women with Recurrent BV") +
  theme(axis.title.x = element_text(size=20), axis.title.y =element_text(size=20), 
        plot.title =element_text(size=25), legend.background = element_rect(size=200)) 