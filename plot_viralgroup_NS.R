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

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#select study_id and nugent
meta <- meta %>% 
  select(study_id, nugent_score_result)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#subset, get 1A
meta <- meta[1:21,]

#merge Nugent data and 1A data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$nugent_score_result)])

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
  select(Participants, Viruses, Counts, nugent_score_result) %>%
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
meta <- read.csv("viromeall_metadata_full.csv")

#select study_id and nugent
meta <- meta %>% 
  select(study_id, nugent_score_result)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#subset, get 1B
meta <- meta[30:54,]

#merge Nugent data and 1A data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$nugent_score_result)])

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
  select(Participants, Viruses, Counts, nugent_score_result) %>%
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
meta <- read.csv("viromeall_metadata_full.csv")

#select sutdy_id and nugent_score_result
meta <- meta %>% 
  select(study_id, nugent_score_result)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#subset, get 1B2
meta <- meta[22:29,]

#merge nugent_score_result data and 1A data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$nugent_score_result)])

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
  select(Participants, Viruses, Counts, nugent_score_result) %>%
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
meta <- read.csv("viromeall_metadata_full.csv")

#select sutdy_id and nugent_score_result
meta <- meta %>% 
  select(study_id, nugent_score_result)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#merge nugent_score_result data and virome data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$nugent_score_result)])

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
  select(Participants, Viruses, Counts, nugent_score_result) %>%
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
#redo with viral groups minus papillomaviridae
#ordered NS
#1A
data <- read.csv("viral_groups_minus_papillomaviridae_1A.csv")
data$X <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")


#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#select sutdy_id and nugent_score_result
meta <- meta %>% 
  select(study_id, nugent_score_result)

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

#merge nugent_score_result data and 1A data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$nugent_score_result)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts',  
         Siphoviridae, Other_Viruses, Other_Phages, Myoviridae, Phycodnaviridae, 
         Podoviridae, Herpesviridae, Other_dsDNA_Viruses, Adenoviridae, Polydnaviridae, 
         Baculoviridae, Polyomaviridae, Picornaviridae, Retroviridae, Other_ssDNA_Viruses, 
         Anelloviridae, Other_dsRNA_Viruses, Mimiviridae, Other_Positive_ssRNA_Viruses, 
         Poxviridae, dsDNA_RT) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, nugent_score_result) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('deepskyblue3', 'gray33', 'deeppink', 'tomato', 
             'purple', 'firebrick', 'forestgreen', 
             'darkgoldenrod1', 'yellow', 'olivedrab2', 'gray',
             'lightsalmon', 'mediumorchid2', 'turquoise', 
             'mediumvioletred', 'green3', 'plum', 'orange', 
             'firebrick1', 'black', 'green')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

#####################################################################################
#1B
data <- read.csv("viral_groups_minus_papillomaviridae_1B.csv")
data$X <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#select sutdy_id and nugent_score_result
meta <- meta %>% 
  select(study_id, nugent_score_result)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#subset, get 1B
meta <- meta[30:54,]

#merge nugent_score_result data and 1A data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$nugent_score_result)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', 
         Siphoviridae, Other_Viruses, Other_Phages, Myoviridae, Phycodnaviridae, 
         Podoviridae, Herpesviridae, Other_dsDNA_Viruses, Adenoviridae, Polydnaviridae, 
         Baculoviridae, Polyomaviridae, Picornaviridae, Retroviridae, Other_ssDNA_Viruses, 
         Anelloviridae, Other_dsRNA_Viruses, Mimiviridae, Other_Positive_ssRNA_Viruses, 
         Poxviridae, dsDNA_RT, Negative_ssRNA_Viruses) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, nugent_score_result) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('deepskyblue3', 'gray33', 'deeppink', 'tomato', 
             'purple', 'firebrick', 'forestgreen', 'slateblue',
             'darkgoldenrod1', 'yellow', 'olivedrab2', 'gray',
             'lightsalmon', 'mediumorchid2', 'turquoise', 
             'mediumvioletred', 'green3', 'plum', 'orange', 
             'firebrick1', 'black', 'green')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

######################################################################################
#1B2
data <- read.csv("viral_groups_minus_papillomaviridae_1B2.csv")
data$X <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#select sutdy_id and nugent_score_result
meta <- meta %>% 
  select(study_id, nugent_score_result)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#subset, get 1B2
meta <- meta[22:29,]

#merge nugent_score_result data and 1A data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$nugent_score_result)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', 
         Siphoviridae, Other_Viruses, Other_Phages, Myoviridae, Phycodnaviridae, 
         Podoviridae, Herpesviridae, Other_dsDNA_Viruses, Adenoviridae, Polydnaviridae, 
         Baculoviridae, Polyomaviridae, Picornaviridae, Retroviridae, Other_ssDNA_Viruses,          Mimiviridae, Other_Positive_ssRNA_Viruses) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, nugent_score_result) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('deepskyblue3', 'deeppink', 
             'purple', 'firebrick', 'forestgreen',
             'darkgoldenrod1', 'olivedrab2', 'gray',
             'lightsalmon', 'mediumorchid2', 'turquoise', 
             'mediumvioletred', 'green3', 'plum', 'orange', 
             'black', 'green')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

####################################################################################
#all
data <- read.csv("viral_groups_minus_papillomaviridae_all.csv")
data$X <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#select sutdy_id and nugent_score_result
meta <- meta %>% 
  select(study_id, nugent_score_result)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#merge nugent_score_result data and virome data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$nugent_score_result)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', 
         Siphoviridae, Other_Viruses, Other_Phages, Myoviridae, Phycodnaviridae, 
         Podoviridae, Herpesviridae, Other_dsDNA_Viruses, Adenoviridae, Polydnaviridae, 
         Baculoviridae, Polyomaviridae, Picornaviridae, Retroviridae, Other_ssDNA_Viruses, 
         Anelloviridae, Other_dsRNA_Viruses, Mimiviridae, Other_Positive_ssRNA_Viruses, 
         Poxviridae, dsDNA_RT, Negative_ssRNA_Viruses) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, nugent_score_result) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('deepskyblue3', 'gray33', 'deeppink', 'tomato', 
             'purple', 'firebrick', 'forestgreen', 'slateblue',
             'darkgoldenrod1', 'yellow', 'olivedrab2', 'gray',
             'lightsalmon', 'mediumorchid2', 'turquoise', 
             'mediumvioletred', 'green3', 'plum', 'orange', 
             'firebrick1', 'black', 'green')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 
