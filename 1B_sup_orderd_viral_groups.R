#1B: Suppressed
data <- read.csv("viral_groups_1B.csv")

#suppressed women
sup <- data %>%
  select(Viruses, Vogue1B.01.03, Vogue1B.01.04, Vogue1B.01.05, Vogue1B.01.06, 
         Vogue1B.01.09, Vogue1B.01.13, Vogue1B.01.15, 
         Vogue1B.01.17, Vogue1B.01.27, Vogue1B.01.34, Vogue1B.01.36, 
         Vogue1B.01.37, 
         Vogue1B.01.38, Vogue1B.01.48, Vogue1B.01.51,Vogue1B.01.52)

#reformat
rownames(sup) <- sup[,1]
sup[,1] <- NULL
sup <- as.data.frame(t(sup))
sup[is.na(sup)] <- 0
sup <- add_rownames(sup, "Participants")

#call for clustering
clust7 <- read.csv("data_clust_7_v2.csv")

#only want suppressed
clust7 <- clust7[order(clust7$X),]
clust7 <- clust7[c(2:5,7,10:12,15,17:20,23:25),]

#rename header and merge with supressed
clust7 <- dplyr::rename(clust7, Participants = X)

#merge
total <- join(sup, clust7, type="full")

#bac counts
data2 <-
  gather(total, key = 'Viruses', value = 'Counts', Papillomaviridae,
         Siphoviridae, Herpesviridae, Phycodnaviridae, Myoviridae,
         Podoviridae, Retroviridae, Other_Phages, Other_Viruses,
         Other_dsDNA_Viruses, Adenoviridae, Polydnaviridae, Anelloviridae,
         Baculoviridae, Other_Positive_ssRNA_Viruses, Picornaviridae, Mimiviridae,
         Other_dsRNA_Viruses, Negative_ssRNA_Viruses, dsDNA_RT, Poxviridae,
         Other_ssDNA_Viruses, Polyomaviridae)

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viruses, Counts, Group) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

# #order by factor levels
vmb$Participants <- factor(vmb$Participants, 
                              levels = vmb$Participants[order(vmb$Group)])


# #order viral groups by abundance
# abundance_order <- vmb %>% group_by(Viruses) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viruses']]
# 
# #Dean added to order by factor levels
# vmb$Viruses <- factor(vmb$Viruses, 
#                       levels = abundance_order)

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