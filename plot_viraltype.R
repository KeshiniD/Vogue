#plot
#call data
everyone <- read.csv("viral_type_all.csv")
vogueA <- read.csv("viral_type_1A.csv")
vogueB <- read.csv("viral_type_1B.csv")
vogue1b2 <- read.csv("viral_type_1B2.csv")

everyone$X <- NULL
vogueA$X <- NULL
vogueB$X <- NULL
vogue1b2$X <- NULL

rownames(everyone) <- everyone[,1]
everyone[,1] <- NULL
everyone2 <- as.data.frame(t(everyone))
everyone2[is.na(everyone2)] <- 0
everyone2 <- add_rownames(everyone2, "Participants")

#bac counts
data <-
  gather(everyone2, key = 'Viral_Types', value = 'Counts', Viral_DNA, 
         Phage, unclassified_viruses, ssRNA_RT, Viral_RNA, dsDNA_RT)

vmb <- tbl_df(data) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Types, Counts) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Types) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viral_Types']]

#Dean added to order by factor levels
vmb$Viral_Types <- factor(vmb$Viral_Types, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('purple', 'green', 'blue', 'orange', 'red', 'yellow')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Types)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") +ggtitle("Metagenomic Characterization of the Vaginal Virome") + 
  theme(axis.title.x = element_text(size=30), axis.title.y =element_text(size=30), 
        plot.title =element_text(size=40), legend.background = element_rect(size=200)) 
########################################################################################
#plot freq
#1A
vogueA <- read.csv("viral_type_1A.csv")
vogueA$X <- NULL
rownames(vogueA) <- vogueA[,1] #have to make sure dataframe is all numeric for rowsums
vogueA[,1] <- NULL
vogueA2 <- vogueA %>%
  mutate(freq = rowSums(vogueA)) #calculating total sum for each viral type
vogueA <- read.csv("viral_type_1A.csv")
vogueA$X <- NULL
vogueA <- join(vogueA, vogueA2, type="full") #merge with orginal dataframe to keep freq & type col.

vogueA <- dplyr::rename(vogueA, Healthy_Asymptomatic = freq)

vogueA <- vogueA %>%
  select(Viral_Type, Healthy_Asymptomatic)

rownames(vogueA) <- vogueA[,1]
vogueA[,1] <- NULL
vogueA2 <- as.data.frame(t(vogueA))
vogueA2[is.na(vogueA2)] <- 0
vogueA2 <- add_rownames(vogueA2, "Cohort")

#1B
vogueB <- read.csv("viral_type_1B.csv")
vogueB$X <- NULL
rownames(vogueB) <- vogueB[,1]
vogueB[,1] <- NULL
vogueB2 <- vogueB %>%
  mutate(freq = rowSums(vogueB))
vogueB <- read.csv("viral_type_1B.csv")
vogueB$X <- NULL
vogueB <- join(vogueB, vogueB2, type="full")

vogueB <- dplyr::rename(vogueB, HIV_Positive = freq)

vogueB <- vogueB %>%
  select(Viral_Type, HIV_Positive)

rownames(vogueB) <- vogueB[,1]
vogueB[,1] <- NULL
vogueB2 <- as.data.frame(t(vogueB))
vogueB2[is.na(vogueB2)] <- 0
vogueB2 <- add_rownames(vogueB2, "Cohort")

#1B2
vogue1B2 <- read.csv("viral_type_1B2.csv")
vogue1B2$X <- NULL
rownames(vogue1B2) <- vogue1B2[,1]
vogue1B2[,1] <- NULL
vogue1B2b <- vogue1B2 %>%
  mutate(freq = rowSums(vogue1B2))
vogue1B2 <- read.csv("viral_type_1B2.csv")
vogue1B2$X <- NULL
vogue1B2 <- join(vogue1B2, vogue1B2b, type="full")

vogue1B2 <- dplyr::rename(vogue1B2, Recurrent_BV = freq)

vogue1B2 <- vogue1B2 %>%
  select(Viral_Type, Recurrent_BV)

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
  gather(total, key = 'Viral_Types', value = 'Counts', Viral_DNA, Phage, 
         unclassified_viruses, Viral_RNA, ssRNA_RT, dsDNA_RT)

vmb <- tbl_df(data) %>% # finally got the percentages correct
  group_by(Cohort) %>%
  select(Cohort, Viral_Types, Counts) %>%
  mutate(Relative.Abundance = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Cohort)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Types) %>% summarize(count = mean(Relative.Abundance)) %>% arrange(desc(count)) %>% .[['Viral_Types']]

#Dean added to order by factor levels
vmb$Viral_Types <- factor(vmb$Viral_Types, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('purple', 'green', 'blue', 'red', 'orange', 'yellow')

ggplot(data = vmb, aes(x = Cohort, y = Relative.Abundance, fill = Viral_Types)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) + 
  ggtitle("Metagenomic Characterization of the Vaginal Virome") + ylab("Relative Abundance (%)") +
  theme(axis.title.x = element_text(size=20), axis.title.y =element_text(size=20), 
        plot.title =element_text(size=25), legend.background = element_rect(size=100), 
        axis.text.y = element_text(size=15)) 

######################################################################################
#plot
#call data
vogueA <- read.csv("viral_type_1A.csv")
vogueB <- read.csv("viral_type_1B.csv")
vogue1b2 <- read.csv("viral_type_1B2.csv")

vogueA$X <- NULL
vogueB$X <- NULL
vogue1b2$X <- NULL

rownames(vogueA) <- vogueA[,1]
vogueA[,1] <- NULL
vogueA <- as.data.frame(t(vogueA))
vogueA[is.na(vogueA)] <- 0
vogueA <- add_rownames(vogueA, "Participants")

#bac counts
data <-
  gather(vogueA, key = 'Viral_Types', value = 'Counts', Viral_DNA, Phage, 
         Viral_RNA, unclassified_viruses, ssRNA_RT, dsDNA_RT)

vmb <- tbl_df(data) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Types, Counts) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Types) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viral_Types']]

#Dean added to order by factor levels
vmb$Viral_Types <- factor(vmb$Viral_Types, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('purple', 'green', 'red', 'blue', 'orange', 'yellow')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Types)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") + 
  ggtitle("Metagenomic Characterization of the Vaginal Virome of Healthy-Asymptomatic Women") +
  theme(axis.title.x = element_text(size=25), axis.title.y =element_text(size=25), 
        plot.title =element_text(size=20), legend.background = element_rect(size=200)) 

#########################################################################################
vogueB <- read.csv("viral_type_1B.csv")
vogue1b2 <- read.csv("viral_type_1B2.csv")

vogueB$X <- NULL
vogueB$freq <- NULL

rownames(vogueB) <- vogueB[,1]
vogueB[,1] <- NULL
vogueB <- as.data.frame(t(vogueB))
vogueB[is.na(vogueB)] <- 0
vogueB <- add_rownames(vogueB, "Participants")

#bac counts
data <-
  gather(vogueB, key = 'Viral_Types', value = 'Counts', Viral_DNA, Phage, 
         unclassified_viruses, ssRNA_RT, Viral_RNA, dsDNA_RT)

vmb <- tbl_df(data) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Types, Counts) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Types) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viral_Types']]

#Dean added to order by factor levels
vmb$Viral_Types <- factor(vmb$Viral_Types, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('purple', 'green', 'blue', 'orange', 'red', 'yellow')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Types)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") + 
  ggtitle("Metagenomic Characterization of the Vaginal Virome of HIV Positive Women") +
  theme(axis.title.x = element_text(size=20), axis.title.y =element_text(size=20), 
        plot.title =element_text(size=25), legend.background = element_rect(size=200)) 

###########################################################################################
vogue1b2 <- read.csv("viral_type_1B2.csv")

vogue1b2$X <- NULL
vogue1b2$freq <- NULL

rownames(vogue1b2) <- vogue1b2[,1]
vogue1b2[,1] <- NULL
vogue1b2 <- as.data.frame(t(vogue1b2))
vogue1b2[is.na(vogue1b2)] <- 0
vogue1b2 <- add_rownames(vogue1b2, "Participants")

#bac counts
data <-
  gather(vogue1b2, key = 'Viral_Types', value = 'Counts', Viral_DNA, Phage, 
         unclassified_viruses, Viral_RNA, ssRNA_RT)

vmb <- tbl_df(data) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Types, Counts) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Types) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viral_Types']]

#Dean added to order by factor levels
vmb$Viral_Types <- factor(vmb$Viral_Types, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('purple', 'green', 'blue', 'red', 'orange')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Types)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") + 
  ggtitle("Metagenomic Characterization of the Vaginal Virome of Women with Recurrent BV") +
  theme(axis.title.x = element_text(size=20), axis.title.y =element_text(size=20), 
        plot.title =element_text(size=25), legend.background = element_rect(size=200)) 
