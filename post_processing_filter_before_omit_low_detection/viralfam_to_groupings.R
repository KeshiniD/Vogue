#redone for filtered files; Sept28-16
#load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)

#call data and recode for DNa, RNA and phage
everyone <- read.csv("DNA_RNA_phage_viral_species_and_family_all.csv", stringsAsFactors = FALSE)
vogueA <- read.csv("DNA_RNA_phage_viral_species_and_family_1A.csv", stringsAsFactors = FALSE)
vogueB <- read.csv("DNA_RNA_phage_viral_species_and_family_1B.csv", stringsAsFactors = FALSE)
vogue1b2 <- read.csv("DNA_RNA_phage_viral_species_and_family_1B2.csv", stringsAsFactors = FALSE)

#omit empty column
everyone$X <- NULL
vogueA$X <- NULL
vogueB$X <- NULL
vogue1b2$X <- NULL

#ref manual
vref <- read.csv("recode_family_to_groupings.csv", header=FALSE, stringsAsFactors = FALSE)

#omit empty rows and columns
# vref <- vref[c(1:72),]

#recode families to virus order
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
everyone2 <- dplyr::rename(everyone2, Virus_Groups = Viral_Family)
vogueA2 <- dplyr::rename(vogueA2, Virus_Groups = Viral_Family)
vogueB2 <- dplyr::rename(vogueB2, Virus_Groups = Viral_Family)
vogue1b2a <- dplyr::rename(vogue1b2a, Virus_Groups = Viral_Family)

#gathering like types                    
everyone3 <- ddply(everyone2,c("Virus_Groups"),numcolwise(sum)) #includes all columns
vogueA3 <- ddply(vogueA2,c("Virus_Groups"),numcolwise(sum)) #includes all columns
vogueB3 <- ddply(vogueB2,c("Virus_Groups"),numcolwise(sum)) #includes all columns
vogue1b2b <- ddply(vogue1b2a,c("Virus_Groups"),numcolwise(sum)) #includes all columns

#na to zero
everyone3[is.na(everyone3)] <- 0
vogueA3[is.na(vogueA3)] <- 0
vogueB3[is.na(vogueB3)] <- 0
vogue1b2b[is.na(vogue1b2b)] <- 0

#write to file
# write.csv(everyone3, "virus_groupings_all.csv")
# write.csv(vogueA3, "virus_groupings_1A.csv")
# write.csv(vogueB3, "virus_groupings_1B.csv")
# write.csv(vogue1b2b, "virus_groupings_1B2.csv")

###########################################################################################
#all
data <- read.csv("virus_groupings_all.csv")

data$X <- NULL
data$freq <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#bac counts
data2 <-
  gather(data, key = 'Viral_Groups', value = 'Counts', Adenoviridae, Anelloviridae, 
         Baculoviridae, Caudovirales, Coccolithovirus, dsDNA, dsDNA_RT, dsRNA, 
         Herpesviridae, Human_papillomavirus_type_56, Mimiviridae, negative_ssRNA, 
         Other_Phages, Other_Viruses_unclassified, Papillomaviridae, Phycodnaviridae,
         Picornaviridae, Podoviridae, Polydnaviridae, Polyomaviridae, positive_ssRNA, 
         Retroviridae, Siphoviridae, ssDNA) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
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
jColors <- c('blue', 'orange', 'purple', 'green', 'green3', 
             'forestgreen', 'deepskyblue3', 'darkgoldenrod1', 
             'cornflowerblue', 'mediumorchid2', 'plum', 'firebrick', 'deeppink',
             'firebrick1', 'gray33', 'gray', 'mediumvioletred', 'black', 'olivedrab2', 
             'yellow', 'tomato', 'lightsalmon', 'slateblue', 'turquoise')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") +
ggtitle("Metagenomic Characterization of the Vaginal Virome of Women") +
  theme(axis.title.x = element_text(size=20), axis.title.y =element_text(size=20),
        plot.title =element_text(size=25), legend.background = element_rect(size=200))

###################################################################################
#1A
data <- read.csv("virus_groupings_1A.csv")

data$X <- NULL
data$freq <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#bac counts
data2 <-
  gather(data, key = 'Viral_Groups', value = 'Counts', Adenoviridae, Anelloviridae, 
         Baculoviridae, Caudovirales, Coccolithovirus, dsDNA, dsDNA_RT, dsRNA, 
         Herpesviridae, Human_papillomavirus_type_56, Mimiviridae,  
         Other_Phages, Other_Viruses_unclassified, Papillomaviridae, Phycodnaviridae,
         Picornaviridae, Podoviridae, Polydnaviridae, Polyomaviridae, positive_ssRNA, 
         Retroviridae, Siphoviridae, ssDNA) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
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
jColors <- c('blue', 'green', 'orange', 'purple', 'green3', 
             'forestgreen', 'mediumorchid2', 'firebrick1', 
             'deeppink', 'plum', 'deepskyblue3', 'cornflowerblue', 'gray33',
             'darkgoldenrod1', 'gray', 'firebrick', 'olivedrab2', 'yellow', 
             'black', 'lightsalmon', 'tomato', 'slateblue', 'mediumvioletred')


ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

####################################################################################
#1B
data <- read.csv("virus_groupings_1B.csv")

data$X <- NULL
data$freq <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#bac counts
data2 <-
  gather(data, key = 'Viral_Groups', value = 'Counts', Adenoviridae, Anelloviridae, 
         Baculoviridae, Caudovirales, Coccolithovirus, dsDNA, dsDNA_RT, dsRNA, 
         Herpesviridae, Human_papillomavirus_type_56, Mimiviridae, negative_ssRNA, 
         Other_Phages, Other_Viruses_unclassified, Papillomaviridae, Phycodnaviridae,
         Picornaviridae, Podoviridae, Polydnaviridae, Polyomaviridae, positive_ssRNA, 
         Retroviridae, Siphoviridae, ssDNA) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
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
jColors <- c('blue', 'orange', 'purple', 'black', 'green', 
             'lightsalmon', 'green3', 'cornflowerblue', 
             'deepskyblue3', 'darkgoldenrod1', 'plum', 'forestgreen', 'mediumorchid2',             'firebrick1', 'firebrick', 'deeppink', 
             'gray', 'gray33', 'tomato', 'olivedrab2', 'mediumvioletred', 'turquoise', 
             'yellow', 'slateblue')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 
####################################################################################
#1B2
data <- read.csv("virus_groupings_1B2.csv")

data$X <- NULL
data$freq <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#bac counts
data2 <-
  gather(data, key = 'Viral_Groups', value = 'Counts', Adenoviridae, Anelloviridae, 
         Baculoviridae, Caudovirales, Coccolithovirus, dsDNA, dsRNA, 
         Herpesviridae, Human_papillomavirus_type_56, Mimiviridae,  
         Other_Phages, Other_Viruses_unclassified, Papillomaviridae, Phycodnaviridae,
         Picornaviridae, Podoviridae, Polydnaviridae, Polyomaviridae, positive_ssRNA, 
         Retroviridae, Siphoviridae, ssDNA) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
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
jColors <- c('blue', 'lightsalmon', 'orange', 'forestgreen', 'green3', 
             'purple', 'black', 'green', 
             'deepskyblue3', 'firebrick1', 'plum', 'mediumorchid2', 'mediumvioletred', 'cornflowerblue', 'deeppink', 'darkgoldenrod1', 'gray', 'gray33', 'firebrick', 'olivedrab2', 'slateblue', 'tomato')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

####################################################################################
#1B: Suppressed vs. Unsupressed
data <- read.csv("virus_groupings_1B.csv")

#unsuppressed women
unsup <- data %>%
  select(Virus_Groups, Vogue1B.01.11, Vogue1B.01.40, Vogue1B.01.01, Vogue1B.01.43, Vogue1B.01.26, Vogue1B.01.32, Vogue1B.01.12, Vogue1B.01.21, Vogue1B.01.08)

#suppressed women
sup <- data %>%
  select(Virus_Groups, Vogue1B.01.03, Vogue1B.01.04, Vogue1B.01.05, Vogue1B.01.06, 
         Vogue1B.01.09, Vogue1B.01.13, Vogue1B.01.15, 
         Vogue1B.01.17, Vogue1B.01.27, Vogue1B.01.34, Vogue1B.01.36, Vogue1B.01.37, 
         Vogue1B.01.38, Vogue1B.01.48, Vogue1B.01.51,Vogue1B.01.52)

####suppressed###
###########################
rownames(sup) <- sup[,1]
sup[,1] <- NULL
sup <- as.data.frame(t(sup))
sup[is.na(sup)] <- 0
sup <- add_rownames(sup, "Participants")

#bac counts
data2 <-
  gather(sup, key = 'Viral_Groups', value = 'Counts', Adenoviridae, Anelloviridae, 
         Baculoviridae, Caudovirales, Coccolithovirus, dsDNA, dsDNA_RT, dsRNA, 
         Herpesviridae, Human_papillomavirus_type_56, Mimiviridae, negative_ssRNA, 
         Other_Phages, Other_Viruses_unclassified, Papillomaviridae, Phycodnaviridae,
         Picornaviridae, Podoviridae, Polydnaviridae, Polyomaviridae, positive_ssRNA, 
         Retroviridae, Siphoviridae, ssDNA) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
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
jColors <- c('blue', 'orange', 'purple', 'black', 'green', 
             'lightsalmon', 'green3', 'darkgoldenrod1', 'cornflowerblue', 'forestgreen', 
             'mediumorchid2', 'deepskyblue3', 'firebrick', 'plum', 'firebrick1', 
             'gray33', 'gray', 'yellow', 'deeppink', 'olivedrab2',  'turquoise',
             'tomato', 'mediumvioletred', 'slateblue')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

###unsuppressed####
##################
rownames(unsup) <- unsup[,1]
unsup[,1] <- NULL
unsup <- as.data.frame(t(unsup))
unsup[is.na(unsup)] <- 0
unsup <- add_rownames(unsup, "Participants")

#bac counts
data2 <-
  gather(unsup, key = 'Viral_Groups', value = 'Counts', Adenoviridae, Anelloviridae, 
         Baculoviridae, Caudovirales, Coccolithovirus, dsDNA, dsDNA_RT, dsRNA, 
         Herpesviridae, Human_papillomavirus_type_56, Mimiviridae, negative_ssRNA, 
         Other_Phages, Other_Viruses_unclassified, Papillomaviridae, Phycodnaviridae,
         Picornaviridae, Podoviridae, Polydnaviridae, Polyomaviridae, positive_ssRNA, 
         Retroviridae, Siphoviridae, ssDNA) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
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
jColors <- c('blue', 'orange', 'purple', 'deepskyblue3', 'cornflowerblue', 
             'black', 'lightsalmon', 'plum', 'green', 'firebrick1', 
             'deeppink', 'green3', 'forestgreen', 'firebrick', 'mediumorchid2', 
             'darkgoldenrod1', 'olivedrab2', 'gray', 'mediumvioletred', 'yellow', 
             'slateblue', 'gray33', 'tomato', 'turquoise')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

#### order based on viral load
#Dean added to order by factor levels
data <- read.csv("virus_groupings_1B.csv")

#unsuppressed women
unsup <- data %>%
  select(Virus_Groups, Vogue1B.01.11, Vogue1B.01.40, Vogue1B.01.01, Vogue1B.01.43, Vogue1B.01.26, Vogue1B.01.32, Vogue1B.01.12, Vogue1B.01.21, Vogue1B.01.08)

#load Viral Load data
vl <- read.csv("viromeall_metadata_full.csv")

#omit empty rows and columns
vl <- vl %>% select(study_id, VL..copies.mL..)
vl <- vl[c(30:54),]

rownames(vl) <- vl[,1] #flip dataframe so can select for only unsup women & then flip back
vl[,1] <- NULL
vl <- as.data.frame(t(vl))

vl2 <- vl %>%
  select(Vogue1B.01.11, Vogue1B.01.40, Vogue1B.01.01, Vogue1B.01.43, Vogue1B.01.26, Vogue1B.01.32, Vogue1B.01.12, Vogue1B.01.21, Vogue1B.01.08)

vl2 <- as.data.frame(t(vl2))
vl2 <- add_rownames(vl2, "Participants")

# #rename study_ids
# vl2$Participants <-
#   paste0("Vogue1B.0",
#          substring(vl2$Participants, nchar("Vogue1B.")+1))

#reorganize dataframe
rownames(unsup) <- unsup[,1]
unsup[,1] <- NULL
unsup <- as.data.frame(t(unsup))
unsup[is.na(unsup)] <- 0
unsup <- add_rownames(unsup, "Participants")

#merge VL data and 1B unsuppressed data
total <- join(vl2, unsup, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$VL..copies.mL..)])

#bac counts
data2 <-
  gather(total, key = 'Viral_Groups', value = 'Counts', Adenoviridae, Anelloviridae, 
         Baculoviridae, Caudovirales, Coccolithovirus, dsDNA, dsDNA_RT, dsRNA, 
         Herpesviridae, Human_papillomavirus_type_56, Mimiviridae, negative_ssRNA, 
         Other_Phages, Other_Viruses_unclassified, Papillomaviridae, Phycodnaviridae,
         Picornaviridae, Podoviridae, Polydnaviridae, Polyomaviridae, positive_ssRNA, 
         Retroviridae, Siphoviridae, ssDNA) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Groups, Counts, VL..copies.mL..) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('forestgreen', 'firebrick1', 'firebrick', 'olivedrab2', 'green', 
             'black', 'tomato', 'yellow', 'purple', 'deepskyblue3', 'deeppink', 
             'turquoise', 'plum', 'cornflowerblue', 
             'blue', 'green3', 'gray', 'lightsalmon', 'mediumorchid2', 'mediumvioletred', 
             'gray33', 'darkgoldenrod1', 'orange', 'slateblue')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

###########################################################################
#order based on CST
#1A
data <- read.csv("virus_groupings_1A.csv")

data$X <- NULL
data$freq <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")


#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#select sutdy_id and CST
meta <- meta %>% 
  select(study_id, CST)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#subset, get 1A
meta <- meta[1:21,]

#rename study_ids
# meta$Participants <-
#   paste0("Vogue1A.01.",
#          substring(meta$Participants, nchar("Vogue 1A 01-0")+1))
# 
# meta$Participants[meta$Participants=='Vogue1A.01.01'] <- 'Vogue1A.01.101'
# meta$Participants[meta$Participants=='Vogue1A.01.06'] <- 'Vogue1A.01.106'

#merge CST data and 1A data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST)])

#bac counts
data2 <-
  gather(total, key = 'Viral_Groups', value = 'Counts', Adenoviridae, Anelloviridae, 
         Baculoviridae, Caudovirales, Coccolithovirus, dsDNA, dsDNA_RT, dsRNA, 
         Herpesviridae, Human_papillomavirus_type_56, Mimiviridae,  
         Other_Phages, Other_Viruses_unclassified, Papillomaviridae, Phycodnaviridae,
         Picornaviridae, Podoviridae, Polydnaviridae, Polyomaviridae, positive_ssRNA, 
         Retroviridae, Siphoviridae, ssDNA) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Groups, Counts, CST) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('forestgreen', 'firebrick1', 'firebrick', 'olivedrab2', 'green', 
             'black', 'tomato', 'yellow', 'purple', 'deepskyblue3', 'deeppink', 
             'plum', 'cornflowerblue', 
             'blue', 'green3', 'gray', 'lightsalmon', 'mediumorchid2', 'mediumvioletred', 
             'gray33', 'darkgoldenrod1', 'orange', 'slateblue')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

####################################################################################
#1B
data <- read.csv("virus_groupings_1B.csv")

data$X <- NULL
data$freq <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#select sutdy_id and CST
meta <- meta %>% 
  select(study_id, CST)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#subset, get 1B
meta <- meta[30:54,]

#rename study_ids
# meta$Participants <-
#   paste0("Vogue1B.01.",
#          substring(meta$Participants, nchar("Vogue1B.01.0")+1))

#merge CST data and 1A data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST)])

#bac counts
data2 <-
  gather(total, key = 'Viral_Groups', value = 'Counts', Adenoviridae, Anelloviridae, 
         Baculoviridae, Caudovirales, Coccolithovirus, dsDNA, dsDNA_RT, dsRNA, 
         Herpesviridae, Human_papillomavirus_type_56, Mimiviridae, negative_ssRNA, 
         Other_Phages, Other_Viruses_unclassified, Papillomaviridae, Phycodnaviridae,
         Picornaviridae, Podoviridae, Polydnaviridae, Polyomaviridae, positive_ssRNA, 
         Retroviridae, Siphoviridae, ssDNA) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Groups, Counts, CST) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('forestgreen', 'firebrick1', 'firebrick', 'olivedrab2', 'green', 
             'black', 'tomato', 'yellow', 'purple', 'deepskyblue3', 'deeppink', 
             'turquoise', 'plum', 'cornflowerblue', 
             'blue', 'green3', 'gray', 'lightsalmon', 'mediumorchid2', 'mediumvioletred', 
             'gray33', 'darkgoldenrod1', 'orange', 'slateblue')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 
####################################################################################
#1B2
data <- read.csv("virus_groupings_1B2.csv")

data$X <- NULL
data$freq <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#select sutdy_id and CST
meta <- meta %>% 
  select(study_id, CST)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#subset, get 1B2
meta <- meta[22:29,]

#rename study_ids
# meta$Participants <-
#   paste0("Vogue1B2.01.",
#          substring(meta$Participants, nchar("Vogue 1B2 01-")+1))

#merge CST data and 1A data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST)])

#bac counts
data2 <-
  gather(total, key = 'Viral_Groups', value = 'Counts', Adenoviridae, Anelloviridae, 
         Baculoviridae, Caudovirales, Coccolithovirus, dsDNA, dsRNA, 
         Herpesviridae, Human_papillomavirus_type_56, Mimiviridae,  
         Other_Phages, Other_Viruses_unclassified, Papillomaviridae, Phycodnaviridae,
         Picornaviridae, Podoviridae, Polydnaviridae, Polyomaviridae, positive_ssRNA, 
         Retroviridae, Siphoviridae, ssDNA) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Groups, Counts, CST) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('forestgreen', 'firebrick1', 'firebrick', 'olivedrab2', 'green', 
             'black', 'yellow', 'purple', 'deepskyblue3', 'deeppink', 'plum', 'cornflowerblue', 
             'blue', 'green3', 'gray', 'lightsalmon', 'mediumorchid2', 'mediumvioletred', 
             'gray33', 'darkgoldenrod1', 'orange', 'slateblue')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

####################################################################################
#all
#order based on CST
data <- read.csv("virus_groupings_all.csv")

data$X <- NULL
data$freq <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")


#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#select sutdy_id and CST
meta <- meta %>% 
  select(study_id, CST)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#merge CST data and virome data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST)])

#bac counts
data2 <-
  gather(total, key = 'Viral_Groups', value = 'Counts', Adenoviridae, Anelloviridae, 
         Baculoviridae, Caudovirales, Coccolithovirus, dsDNA, dsDNA_RT, dsRNA, 
         Herpesviridae, Human_papillomavirus_type_56, Mimiviridae,  
         Other_Phages, Other_Viruses_unclassified, Papillomaviridae, Phycodnaviridae,
         Picornaviridae, Podoviridae, Polydnaviridae, Polyomaviridae, positive_ssRNA, 
         Retroviridae, Siphoviridae, ssDNA) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Groups, Counts, CST) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('forestgreen', 'firebrick1', 'firebrick', 'olivedrab2', 'green', 
             'black', 'tomato', 'yellow', 'purple', 'deepskyblue3', 'deeppink', 
             'plum', 'cornflowerblue', 
             'blue', 'green3', 'gray', 'lightsalmon', 'mediumorchid2', 'mediumvioletred', 
             'gray33', 'darkgoldenrod1', 'orange', 'slateblue')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 
