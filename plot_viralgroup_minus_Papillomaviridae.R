#did all, and now will do profiles ordered by CSTs, VL, and the diff CST group
#all
data <- read.csv("viral_groups_minus_papillomaviridae_all.csv")
data$X <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#bac counts
data2 <-
  gather(data, key = 'Viruses', value = 'Counts', Siphoviridae, Phycodnaviridae, 
         Herpesviridae, Podoviridae, Myoviridae, 
         Adenoviridae, Other_Phages, Polydnaviridae, Other_dsDNA_Viruses, Other_Viruses, 
         Mimiviridae, Retroviridae, Picornaviridae, Baculoviridae, Anelloviridae, 
         Other_Positive_ssRNA_Viruses, Negative_ssRNA_Viruses, Polyomaviridae, 
         Poxviridae, Other_dsRNA_Viruses, Other_ssDNA_Viruses, dsDNA_RT) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viruses) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viruses']]

#Dean added to order by factor levels
vmb$Viruses <- factor(vmb$Viruses, 
                      levels = abundance_order)

#bar plot with custom colors
jColors <- c('green', 'turquoise', 'purple', 'green3', 
             'forestgreen', 'deepskyblue3', 'olivedrab2', 
             'plum', 'darkgoldenrod1', 'mediumorchid2', 'firebrick',
             'black', 'mediumvioletred', 'deeppink', 'gray33', 
             'gray', 'slateblue', 'orange', 'firebrick', 'yellow', 
             'lightsalmon', 'tomato')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") +
  ggtitle("Metagenomic Characterization of the Vaginal Virome of Women") +
  theme(axis.title.x = element_text(size=20), axis.title.y =element_text(size=20),
        plot.title =element_text(size=25), legend.background = element_rect(size=200))

#####################################################################################
######################################################################################
#1B: Suppressed vs. Unsupressed
#OCt5-16: add intermediate group (40-400VL)
data <- read.csv("viral_groups_minus_papillomaviridae_1B.csv")

#unsuppressed women
unsup <- data %>%
  select(Viruses, Vogue1B.01.11, Vogue1B.01.40, Vogue1B.01.01, Vogue1B.01.43, Vogue1B.01.26)

#intermediate group
int <- data %>%
  select(Viruses, Vogue1B.01.08, Vogue1B.01.21, Vogue1B.01.12, Vogue1B.01.32)

#suppressed women
sup <- data %>%
  select(Viruses, Vogue1B.01.03, Vogue1B.01.04, Vogue1B.01.05, Vogue1B.01.06, 
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
  gather(sup, key = 'Viruses', value = 'Counts',  
         Siphoviridae, Herpesviridae, Phycodnaviridae, Myoviridae, 
         Polydnaviridae, Podoviridae, Other_Phages, Retroviridae, 
         Other_dsDNA_Viruses, Other_Viruses, Adenoviridae, 
         Negative_ssRNA_Viruses, Anelloviridae, Baculoviridae, 
         Picornaviridae, Other_Positive_ssRNA_Viruses, Mimiviridae, 
         Other_dsRNA_Viruses, dsDNA_RT, Poxviridae, Polyomaviridae, 
         Other_ssDNA_Viruses) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viruses) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viruses']]

#Dean added to order by factor levels
vmb$Viruses <- factor(vmb$Viruses, 
                      levels = abundance_order)

#bar plot with custom colors
jColors <- c('green', 'purple', 'turquoise', 'forestgreen', 'plum', 
             'green3', 'olivedrab2', 'black', 'darkgoldenrod1', 'mediumorchid2', 
             'deepskyblue3', 'slateblue', 'gray33', 'deeppink', 'mediumvioletred', 
             'gray', 'firebrick', 'yellow', 'tomato', 'firebrick1', 'orange', 
             'lightsalmon')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

###############################################################################
#unsuppressed and intermediate order based on viral load
#Dean added to order by factor levels
# data <- read.csv("viral_groups_1B.csv")
# 
# #unsuppressed women
# unsup <- data %>%
#   select(Virus_Groups, Vogue1B.01.11, Vogue1B.01.40, Vogue1B.01.01, Vogue1B.01.43, Vogue1B.01.26, Vogue1B.01.32, Vogue1B.01.12, Vogue1B.01.21, Vogue1B.01.08)

#load Viral Load data
vl <- read.csv("viromeall_metadata_full.csv")

#omit empty rows and columns
vl <- vl %>% select(study_id, VL..copies.mL..)
vl <- vl[c(30:54),]

rownames(vl) <- vl[,1] #flip dataframe so can select for only unsup women & then flip back
vl[,1] <- NULL
vl <- as.data.frame(t(vl))

vl2 <- vl %>%
  select(Vogue1B.01.11, Vogue1B.01.40, Vogue1B.01.01, Vogue1B.01.43, Vogue1B.01.26)

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
  gather(total, key = 'Viruses', value = 'Counts',  
         Siphoviridae, Other_Viruses, Other_Phages, Myoviridae, Phycodnaviridae, 
         Podoviridae, Herpesviridae, Other_dsDNA_Viruses, Adenoviridae, Polydnaviridae, 
         Baculoviridae, Polyomaviridae, Picornaviridae, Retroviridae, Other_ssDNA_Viruses, 
         Anelloviridae, Other_dsRNA_Viruses, Mimiviridae, Other_Positive_ssRNA_Viruses, 
         Poxviridae, dsDNA_RT, Negative_ssRNA_Viruses) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, VL..copies.mL..) %>%
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

###########################################################################
#ordered based on Vl for intermediate group
vl <- read.csv("viromeall_metadata_full.csv")

#omit empty rows and columns
vl <- vl %>% select(study_id, VL..copies.mL..)
vl <- vl[c(30:54),]

rownames(vl) <- vl[,1] #flip dataframe so can select for only unsup women & then flip back
vl[,1] <- NULL
vl <- as.data.frame(t(vl))

vl2 <- vl %>%
  select(Vogue1B.01.08, Vogue1B.01.21, Vogue1B.01.12, Vogue1B.01.32)

vl2 <- as.data.frame(t(vl2))
vl2 <- add_rownames(vl2, "Participants")

# #rename study_ids
# vl2$Participants <-
#   paste0("Vogue1B.0",
#          substring(vl2$Participants, nchar("Vogue1B.")+1))

#reorganize dataframe
rownames(int) <- int[,1]
int[,1] <- NULL
int <- as.data.frame(t(int))
int[is.na(int)] <- 0
int <- add_rownames(int, "Participants")

#merge VL data and 1B intpressed data
total <- join(vl2, int, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$VL..copies.mL..)])

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
  select(Participants, Viruses, Counts, VL..copies.mL..) %>%
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
#####################################################################################
#order based on CST
#1A
data <- read.csv("viral_groups_minus_papillomaviridae_1A.csv")

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
  gather(total, key = 'Viruses', value = 'Counts',  
         Siphoviridae, Other_Viruses, Other_Phages, Myoviridae, Phycodnaviridae, 
         Podoviridae, Herpesviridae, Other_dsDNA_Viruses, Adenoviridae, Polydnaviridae, 
         Baculoviridae, Polyomaviridae, Picornaviridae, Retroviridae, Other_ssDNA_Viruses, 
         Anelloviridae, Other_dsRNA_Viruses, Mimiviridae, Other_Positive_ssRNA_Viruses, 
         Poxviridae, dsDNA_RT) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, CST) %>%
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

#############################################################################################

#1B
data <- read.csv("viral_groups_minus_papillomaviridae_1B.csv")

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
  gather(total, key = 'Viruses', value = 'Counts', 
         Siphoviridae, Other_Viruses, Other_Phages, Myoviridae, Phycodnaviridae, 
         Podoviridae, Herpesviridae, Other_dsDNA_Viruses, Adenoviridae, Polydnaviridae, 
         Baculoviridae, Polyomaviridae, Picornaviridae, Retroviridae, Other_ssDNA_Viruses, 
         Anelloviridae, Other_dsRNA_Viruses, Mimiviridae, Other_Positive_ssRNA_Viruses, 
         Poxviridae, dsDNA_RT, Negative_ssRNA_Viruses) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, CST) %>%
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

#####################################################################################################

#1B2
data <- read.csv("viral_groups_minus_papillomaviridae_1B2.csv")

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
  gather(total, key = 'Viruses', value = 'Counts', 
         Siphoviridae, Other_Viruses, Other_Phages, Myoviridae, Phycodnaviridae, 
         Podoviridae, Herpesviridae, Other_dsDNA_Viruses, Adenoviridae, Polydnaviridae, 
         Baculoviridae, Polyomaviridae, Picornaviridae, Retroviridae, Other_ssDNA_Viruses,          Mimiviridae, Other_Positive_ssRNA_Viruses) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, CST) %>%
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
#order based on CST
data <- read.csv("viral_groups_minus_papillomaviridae_all.csv")

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
  gather(total, key = 'Viruses', value = 'Counts', 
         Siphoviridae, Other_Viruses, Other_Phages, Myoviridae, Phycodnaviridae, 
         Podoviridae, Herpesviridae, Other_dsDNA_Viruses, Adenoviridae, Polydnaviridae, 
         Baculoviridae, Polyomaviridae, Picornaviridae, Retroviridae, Other_ssDNA_Viruses, 
         Anelloviridae, Other_dsRNA_Viruses, Mimiviridae, Other_Positive_ssRNA_Viruses, 
         Poxviridae, dsDNA_RT, Negative_ssRNA_Viruses) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, CST) %>%
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
######################################################################################
######################################################################################
#ordered based on CSTS, where it's hetero CSTs v. Lacto CSTs
#all
#order based on CST
data <- read.csv("viral_groups_minus_papillomaviridae_all.csv")
data$X <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#reassign CSTs to L: Lacto CST, H: Hetero CST, I: iners
meta$CST.cat[meta$CST=='I' | meta$CST == 'II' | meta$CST == "V"] <- 'L'
meta$CST.cat[meta$CST=='III'] <- 'I'
meta$CST.cat[meta$CST=='IVA' | meta$CST == 'IVC' | meta$CST == "IVD"] <- 'H'

#select sutdy_id and CST
meta <- meta %>% 
  select(study_id, CST, CST.cat)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#merge CST data and virome data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST.cat)])

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
  select(Participants, Viruses, Counts, CST.cat) %>%
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

############################################################################################
#1A
#order based on CST
data <- read.csv("viral_groups_minus_papillomaviridae_1A.csv")
data$X <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#subset, get 1A
meta <- meta[1:21,]

#reassign CSTs to L: Lacto CST, H: Hetero CST, I: iners
meta$CST.cat[meta$CST=='I' | meta$CST == 'II' | meta$CST == "V"] <- 'L'
meta$CST.cat[meta$CST=='III'] <- 'I'
meta$CST.cat[meta$CST=='IVA' | meta$CST == 'IVC' | meta$CST == "IVD"] <- 'H'

#select sutdy_id and CST
meta <- meta %>% 
  select(study_id, CST, CST.cat)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#merge CST data and virome data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST.cat)])

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
  select(Participants, Viruses, Counts, CST.cat) %>%
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

######################
#1B
#order based on CST
data <- read.csv("viral_groups_minus_papillomaviridae_1B.csv")
data$X <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#subset, get 1B
meta <- meta[30:54,]

#reassign CSTs to L: Lacto CST, H: Hetero CST, I: iners
meta$CST.cat[meta$CST=='I' | meta$CST == 'II' | meta$CST == "V"] <- 'L'
meta$CST.cat[meta$CST=='III'] <- 'I'
meta$CST.cat[meta$CST=='IVA' | meta$CST == 'IVC' | meta$CST == "IVD"] <- 'H'

#select sutdy_id and CST
meta <- meta %>% 
  select(study_id, CST, CST.cat)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#merge CST data and virome data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST.cat)])

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
  select(Participants, Viruses, Counts, CST.cat) %>%
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

#################

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

#subset, get 1B2
meta <- meta[22:29,]

#reassign CSTs to L: Lacto CST, H: Hetero CST, I: iners
meta$CST.cat[meta$CST=='I' | meta$CST == 'II' | meta$CST == "V"] <- 'L'
meta$CST.cat[meta$CST=='III'] <- 'I'
meta$CST.cat[meta$CST=='IVA' | meta$CST == 'IVC' | meta$CST == "IVD"] <- 'H'

#select sutdy_id and CST
meta <- meta %>% 
  select(study_id, CST, CST.cat)

#rename
meta <- dplyr::rename(meta, Participants = study_id)

#merge CST data and virome data
total <- join(data, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST.cat)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts',  
         Siphoviridae, Other_Viruses, Other_Phages, Myoviridae, Phycodnaviridae, 
         Podoviridae, Herpesviridae, Other_dsDNA_Viruses, Adenoviridae, Polydnaviridae, 
         Baculoviridae, Polyomaviridae, Picornaviridae, Retroviridae, Other_ssDNA_Viruses, 
         Mimiviridae, Other_Positive_ssRNA_Viruses) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, CST.cat) %>%
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
