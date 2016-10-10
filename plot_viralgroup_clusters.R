##############################################################################
##############################################################################
#order based on Nugent score
#1A
data <- read.csv("viral_groups_1A.csv")
data$X <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#load clust groups
meta <- read.csv("data_clust_7_v2.csv")

#rename
meta <- dplyr::rename(meta, Participants = X)

#subset, get 1A
meta <- meta[order(meta$Participants),]
meta <- meta[1:21,]

#merge Nugent data and 1A data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$Group)])

#viral counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', Papillomaviridae, 
         Siphoviridae, Other_Viruses, Other_Phages, Myoviridae, 
         Phycodnaviridae, Podoviridae, Herpesviridae, Other_dsDNA_Viruses, 
         Adenoviridae, Polydnaviridae, Baculoviridae, Polyomaviridae, 
         Picornaviridae, Retroviridae, Other_ssDNA_Viruses, Anelloviridae, 
         Other_dsRNA_Viruses, Mimiviridae, Other_Positive_ssRNA_Viruses, 
         Poxviridae, dsDNA_RT) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, Group) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('deepskyblue3', 'gray33', 'deeppink', 'tomato', 
             'purple', 'firebrick', 'forestgreen', 
             'darkgoldenrod1', 'yellow', 'olivedrab2', 'gray',
             'lightsalmon', 'mediumorchid2', 'blue', 'turquoise', 
             'mediumvioletred', 'green3', 'plum', 'orange', 
             'firebrick1', 'black', 'green')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

###############################################################################
#1B
data <- read.csv("viral_groups_1B.csv")
data$X <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#load metadata
meta <- read.csv("data_clust_7_v2.csv")

#rename
meta <- dplyr::rename(meta, Participants = X)

#subset, get 1B
meta <- meta[order(meta$Participants),]
meta <- meta[22:46,]

#merge Nugent data and 1A data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$Group)])

#viral counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', Papillomaviridae, 
         Siphoviridae, Other_Viruses, Other_Phages, Myoviridae, 
         Phycodnaviridae, Podoviridae, Herpesviridae, Other_dsDNA_Viruses, 
         Adenoviridae, Polydnaviridae, Baculoviridae, Polyomaviridae, 
         Picornaviridae, Retroviridae, Other_ssDNA_Viruses, Anelloviridae, 
         Other_dsRNA_Viruses, Mimiviridae, Other_Positive_ssRNA_Viruses, 
         Poxviridae, dsDNA_RT, Negative_ssRNA_Viruses) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, Group) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('deepskyblue3', 'gray33', 'deeppink', 'tomato', 
             'purple', 'firebrick', 'forestgreen', 'slateblue',
             'darkgoldenrod1', 'yellow', 'olivedrab2', 'gray',
             'lightsalmon', 'mediumorchid2', 'blue', 'turquoise', 
             'mediumvioletred', 'green3', 'plum', 'orange', 
             'firebrick1', 'black', 'green')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

##########################################################
#1B2
data <- read.csv("viral_groups_1B2.csv")
data$X <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#load metadata
meta <- read.csv("data_clust_7_v2.csv")

#rename
meta <- dplyr::rename(meta, Participants = X)

#subset, get 1B2
meta <- meta[order(meta$Participants),]
meta <- meta[47:54,]

#merge nugent_score_result data and 1A data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$Group)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', Papillomaviridae, 
         Siphoviridae, Other_Viruses, Other_Phages, Myoviridae, 
         Phycodnaviridae, Podoviridae, Herpesviridae, Other_dsDNA_Viruses, 
         Adenoviridae, Polydnaviridae, Baculoviridae, Polyomaviridae, 
         Picornaviridae, Retroviridae, Other_ssDNA_Viruses, Mimiviridae, 
         Other_Positive_ssRNA_Viruses) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, Group) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('deepskyblue3', 'deeppink', 
             'purple', 'firebrick', 'forestgreen',
             'darkgoldenrod1', 'olivedrab2', 'gray',
             'lightsalmon', 'mediumorchid2', 'blue', 'turquoise', 
             'mediumvioletred', 'green3', 'plum', 'orange', 
             'black', 'green')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

########################
#all
#order based on nugent_score_result
data <- read.csv("viral_groups_all.csv")
data$X <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#load metadata
meta <- read.csv("data_clust_7_v2.csv")

#rename
meta <- dplyr::rename(meta, Participants = X)

#merge nugent_score_result data and virome data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$Group)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', Papillomaviridae, 
         Siphoviridae, Other_Viruses, Other_Phages, Myoviridae, Phycodnaviridae, 
         Podoviridae, Herpesviridae, Other_dsDNA_Viruses, Adenoviridae, Polydnaviridae, 
         Baculoviridae, Polyomaviridae, Picornaviridae, Retroviridae, Other_ssDNA_Viruses, 
         Anelloviridae, Other_dsRNA_Viruses, Mimiviridae, Other_Positive_ssRNA_Viruses, 
         Poxviridae, dsDNA_RT, Negative_ssRNA_Viruses) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, Group) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('deepskyblue3', 'gray33', 'deeppink', 'tomato', 
             'purple', 'firebrick', 'forestgreen', 'slateblue',
             'darkgoldenrod1', 'yellow', 'olivedrab2', 'gray',
             'lightsalmon', 'mediumorchid2', 'blue', 'turquoise', 
             'mediumvioletred', 'green3', 'plum', 'orange', 
             'firebrick1', 'black', 'green')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

######################################################################################
########################################################################################
