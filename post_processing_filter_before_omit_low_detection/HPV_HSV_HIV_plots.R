#plot, and order based on CSTs (hetero, iners, and lacto)
#call data
everyone <- read.csv("HPV_HSV_HIV_all.csv")
vogueA <- read.csv("HPV_HSV_HIV_1A.csv")
vogueB <- read.csv("HPV_HSV_HIV_1B.csv")
vogue1b2 <- read.csv("HPV_HSV_HIV_1B2.csv")

#to order alphabetically
# a <- a[order(a$Viral_Species),]

##################################################
#all
everyone <- read.csv("HPV_HSV_HIV_all.csv")

#collapse like species
everyone <- ddply(everyone,c("Viral_Species"),numcolwise(sum)) 

rownames(everyone) <- everyone[,1]
everyone[,1] <- NULL
everyone <- as.data.frame(t(everyone))
everyone[is.na(everyone)] <- 0
everyone <- add_rownames(everyone, "Participants")

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
total <- join(everyone, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST.cat)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', Alphapapillomavirus, 
         Betapapillomavirus, Gammapapillomavirus, Other_Human_Papillomavirus, 
         Other_Papillomaviridae, Human_Immunodeficiency_Virus_1, 
         Human_Herpesvirus_1, Human_Herpesvirus_2, Human_Herpesvirus_4, 
         Lymphocryptovirus, Human_Herpesvirus_5, Cytomegalovirus, 
         Human_Herpesvirus_6, Human_Herpesvirus_6A, Human_Herpesvirus_6B, 
         Roseolovirus, Human_Herpesvirus_8, Other_Herpesviridae) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, CST.cat) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('purple4', 'purple', 'deeppink', 'mediumorchid2', 
             'firebrick', 'firebrick1', 'green', 'pink',
             'darkgoldenrod1', 'orange','yellow', 'blue', 'red',
             'forestgreen', 'turquoise', 'plum', 
             'mediumvioletred', 'yellow4')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

##########################
#fam

everyone <- read.csv("HPV_HSV_HIV_all.csv")

#collapse like species
everyone <- ddply(everyone,c("Viral_Family"),numcolwise(sum)) 

rownames(everyone) <- everyone[,1]
everyone[,1] <- NULL
everyone <- as.data.frame(t(everyone))
everyone[is.na(everyone)] <- 0
everyone <- add_rownames(everyone, "Participants")

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
total <- join(everyone, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST.cat)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', Papillomaviridae, Herpesviridae, Retroviridae) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, CST.cat) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('purple', 'green', 'orange')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

##########################################################################
####1A
vogueA <- read.csv("HPV_HSV_HIV_1A.csv")

#collapse like species
vogueA <- ddply(vogueA,c("Viral_Species"),numcolwise(sum)) 

rownames(vogueA) <- vogueA[,1]
vogueA[,1] <- NULL
vogueA <- as.data.frame(t(vogueA))
vogueA[is.na(vogueA)] <- 0
vogueA <- add_rownames(vogueA, "Participants")

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#subset for 1A
meta <- meta[c(1:21),]

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
total <- join(vogueA, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST.cat)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', Alphapapillomavirus, 
         Betapapillomavirus, Gammapapillomavirus, Other_Human_Papillomavirus, 
         Other_Papillomaviridae, Human_Immunodeficiency_Virus_1, 
         Human_Herpesvirus_4, 
         Lymphocryptovirus, Human_Herpesvirus_5,  
         Human_Herpesvirus_6, Human_Herpesvirus_6A, Human_Herpesvirus_6B, 
         Human_Herpesvirus_8) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, CST.cat) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('purple4', 'purple', 'deeppink', 'mediumorchid2', 
             'firebrick', 'firebrick1', 'green', 'pink',
             'darkgoldenrod1', 'orange','yellow', 'blue', 'red',
             'forestgreen', 'turquoise', 'plum', 
             'mediumvioletred', 'yellow4')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

##########################
#fam
everyone <- read.csv("HPV_HSV_HIV_1A.csv")

#collapse like species
everyone <- ddply(everyone,c("Viral_Family"),numcolwise(sum)) 

rownames(everyone) <- everyone[,1]
everyone[,1] <- NULL
everyone <- as.data.frame(t(everyone))
everyone[is.na(everyone)] <- 0
everyone <- add_rownames(everyone, "Participants")

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#subset for 1A
meta <- meta[c(1:21),]

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
total <- join(everyone, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST.cat)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', Papillomaviridae, Herpesviridae, Retroviridae) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, CST.cat) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('purple', 'green', 'orange')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

###################
#1B
vogueB <- read.csv("HPV_HSV_HIV_1B.csv")

#collapse like species
vogueB <- ddply(vogueB,c("Viral_Species"),numcolwise(sum)) 

rownames(vogueB) <- vogueB[,1]
vogueB[,1] <- NULL
vogueB <- as.data.frame(t(vogueB))
vogueB[is.na(vogueB)] <- 0
vogueB <- add_rownames(vogueB, "Participants")

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#subset for 1B
meta <- meta[c(30:54),]

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
total <- join(vogueB, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST.cat)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', Alphapapillomavirus, 
         Betapapillomavirus, Gammapapillomavirus, Other_Human_Papillomavirus, 
         Other_Papillomaviridae, Human_Immunodeficiency_Virus_1, 
         Human_Herpesvirus_1, Human_Herpesvirus_2, Human_Herpesvirus_4, 
         Lymphocryptovirus, Human_Herpesvirus_5, Cytomegalovirus, 
         Human_Herpesvirus_6A, Human_Herpesvirus_6B, 
         Roseolovirus, Human_Herpesvirus_8, Other_Herpesviridae) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, CST.cat) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('purple4', 'purple', 'deeppink', 'mediumorchid2', 
             'firebrick', 'firebrick1', 'green', 'pink',
             'darkgoldenrod1', 'orange','yellow', 'blue', 'red',
             'forestgreen', 'turquoise', 'plum', 
             'mediumvioletred', 'yellow4')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

##########################
#fam

vogueB <- read.csv("HPV_HSV_HIV_1B.csv")

#collapse like species
vogueB <- ddply(vogueB,c("Viral_Family"),numcolwise(sum)) 

rownames(vogueB) <- vogueB[,1]
vogueB[,1] <- NULL
vogueB <- as.data.frame(t(vogueB))
vogueB[is.na(vogueB)] <- 0
vogueB <- add_rownames(vogueB, "Participants")

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#subset for 1B
meta <- meta[c(30:54),]

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
total <- join(vogueB, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST.cat)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', Papillomaviridae, Herpesviridae, Retroviridae) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, CST.cat) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('purple', 'green', 'orange')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

#############################################
#1B2
vogue1b2 <- read.csv("HPV_HSV_HIV_1B2.csv")

#collapse like species
vogue1b2 <- ddply(vogue1b2,c("Viral_Species"),numcolwise(sum)) 

rownames(vogue1b2) <- vogue1b2[,1]
vogue1b2[,1] <- NULL
vogue1b2 <- as.data.frame(t(vogue1b2))
vogue1b2[is.na(vogue1b2)] <- 0
vogue1b2 <- add_rownames(vogue1b2, "Participants")

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#subset for 1B2
meta <- meta[c(22:29),]

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
total <- join(vogue1b2, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST.cat)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', Alphapapillomavirus, 
         Betapapillomavirus, Gammapapillomavirus, Other_Human_Papillomavirus, 
         Other_Papillomaviridae, Human_Herpesvirus_4, 
         Lymphocryptovirus, Human_Herpesvirus_5, 
         Human_Herpesvirus_6A) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, CST.cat) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('purple4', 'purple', 'deeppink', 'mediumorchid2', 
             'firebrick', 'firebrick1', 'green', 'pink',
             'darkgoldenrod1', 'orange','yellow', 'blue', 'red',
             'forestgreen', 'turquoise', 'plum', 
             'mediumvioletred', 'yellow4')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

##########################
#fam
vogue1b2 <- read.csv("HPV_HSV_HIV_1B2.csv")

#collapse like species
vogue1b2 <- ddply(vogue1b2,c("Viral_Family"),numcolwise(sum)) 

rownames(vogue1b2) <- vogue1b2[,1]
vogue1b2[,1] <- NULL
vogue1b2 <- as.data.frame(t(vogue1b2))
vogue1b2[is.na(vogue1b2)] <- 0
vogue1b2 <- add_rownames(vogue1b2, "Participants")

#load metadata
meta <- read.csv("viromeall_metadata_full.csv")

#subset for 1B2
meta <- meta[c(22:29),]

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
total <- join(vogue1b2, meta, type="full")

#remove NA and replace with zero
total[is.na(total)] <- 0

#order by factor levels
total$Participants <- factor(total$Participants, 
                             levels = total$Participants[order(total$CST.cat)])

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', Papillomaviridae, 
         Herpesviridae) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, CST.cat) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('purple', 'green', 'orange')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viruses)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 