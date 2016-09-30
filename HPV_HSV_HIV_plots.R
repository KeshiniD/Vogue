#plot
#call data
everyone <- read.csv("HPV_HSV_HIV_all.csv")
vogueA <- read.csv("HPV_HSV_HIV_1A.csv")
vogueB <- read.csv("HPV_HSV_HIV_1B.csv")
vogue1b2 <- read.csv("HPV_HSV_HIV_1B2.csv")

#to order alphabetically
# a <- a[order(a$Viral_Species),]

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


#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Groups) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viral_Groups']]

#Dean added to order by factor levels
vmb$Viral_Groups <- factor(vmb$Viral_Groups, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('forestgreen', 'firebrick1', 'firebrick', 'olivedrab2', 'green', 
             'black', 'tomato', 'yellow', 'purple', 'deepskyblue3', 'deeppink', 
             'plum', 'cornflowerblue', 
             'blue', 'green3', 'gray', 'lightsalmon', 'mediumorchid2', 'mediumvioletred', 
             'gray33', 'darkgoldenrod1', 'orange', 'slateblue')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Groups)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 