#trying to organize bac profiles via Nugent.score
#call for data
data <- read.csv(file.path("1B2.csv"))

#add new column with CST.participants
data[,"Vogue.Participants"]  <- c('N.2.Vogue1B2.01.01', 'N.4.Vogue1B2.01.06', 
                                  'N.4.Vogue1B2.01.07', 'N.6.Vogue1B2.01.08', 
                                  'N.0.Vogue1B2.01.09', 'N.7.Vogue1B2.01.10', 
                                  'N.5.Vogue1B2.01.11', 'N.4.Vogue1B2.01.12', 
                                  'N.0.Vogue1B2.01.15', 'N.1.Vogue1B2.01.19', 
                                  'N.6.Vogue1B2.01.21', 'N.4.Vogue1B2.01.23', 
                                  'N.8.Vogue1B2.01.26', 'N.4.Vogue1B2.01.28', 
                                  'N.4.Vogue1B2.01.29', 'N.6.Vogue1B2.01.35', 
                                  'N.0.Vogue1B2.01.37', 'N.8.Vogue1B2.01.38', 
                                  'N.4.Vogue1B2.01.50', 'N.6.Vogue1B2.01.52', 
                                  'N.8.Vogue1B2.01.56', 'N.4.Vogue1B2.01.58', 
                                  'N.8.Vogue1B2.01.61', 'N.7.Vogue1B2.01.62', 
                                  'N.0.Vogue1B2.01.63', 'N.7.Vogue1B2.01.64')
#bac counts
data2 <-
  gather(data, key = 'Bacteria', value = 'Counts', Lactobacillus.crispatus, 
         Lactobacillus.gasseri, Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, Eukaryote,
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Vogue.Participants) %>%
  select(Participants, Bacteria, Counts) %>%
  mutate(Species.Percentage = Counts/(sum(Counts))*100) %>% 
  # can either have % or decimal
  arrange(Vogue.Participants)

#bar plot with custom colors
jColors <- c('darkgoldenrod1', 
             'purple', 'mediumorchid2', 'plum', 'firebrick', 'yellow', 'green3', 
             'forestgreen', 'palegreen', 'green',
             'firebrick1', 'blue', 'deepskyblue3', 'cornflowerblue', 'deepskyblue',
             'gray33', 'gray', 'mediumvioletred', 'black', 'olivedrab2', 
             'orange3', 'tomato', 'lightsalmon', 'slateblue', 'turquoise', 
             'lavender', 'rosybrown2', 'deeppink')

ggplot(data = vmb, aes(x = Vogue.Participants, y = Species.Percentage, fill = Bacteria)) + 
  geom_bar(stat = "identity") + coord_flip() + ylab("Species Proportion") +
  scale_fill_manual(values=jColors) + 
  theme(legend.title = element_blank()) + xlab("Participants") + 
  ggtitle("Cpn60 Species Characterization of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis")

##############################
#Aug-5-16
#add Nugent score column in, and order that way

#call for data
data <- read.csv(file.path("1B2.csv"))

#call for metadata
meta <- read.csv("1B2metabac_condensedv2.csv")

#get nugent score column
nugent <- data.frame(meta$Participants, meta$Nugent.score)
nugent <- dplyr::rename(nugent, Participants = meta.Participants, 
                        Nugent.score = meta.Nugent.score) #rename

#join with bacteria
total <- join(nugent, data, type="full")

#bac counts
total2 <-
  gather(total, key = 'Bacteria', value = 'Counts', Lactobacillus.crispatus, 
         Lactobacillus.gasseri, Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, Eukaryote,
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)

vmb <- tbl_df(total2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Bacteria, Counts, Nugent.score) %>%
  mutate(Species.Percentage = Counts/(sum(Counts))*100) %>% 
  # can either have % or decimal
  arrange(Participants)

#Dean added to order by factor levels
vmb$Participants <- factor(vmb$Participants, 
                           levels = vmb$Participants[order(vmb$Nugent.score)])

#bar plot with custom colors
jColors <- c('darkgoldenrod1', 
             'purple', 'mediumorchid2', 'plum', 'firebrick', 'yellow', 'green3', 
             'forestgreen', 'palegreen', 'green',
             'firebrick1', 'blue', 'deepskyblue3', 'cornflowerblue', 'deepskyblue',
             'gray33', 'gray', 'mediumvioletred', 'black', 'olivedrab2', 
             'orange3', 'tomato', 'lightsalmon', 'slateblue', 'turquoise', 
             'lavender', 'rosybrown2', 'deeppink')

ggplot(data = vmb, aes(x = Participants, y = Species.Percentage, fill = Bacteria)) + 
  geom_bar(stat = "identity") + coord_flip() + ylab("Species Proportion") +
  scale_fill_manual(values=jColors) + 
  theme(legend.position = "bottom", legend.key.size = unit(.35, "cm"), 
        legend.title = element_blank(), plot.title = element_text(size=18), 
        axis.text = element_text(size=14), axis.title = element_text(size=18), 
        legend.text = element_text(size=11)) + xlab("Participants") +  
  ggtitle("Cpn60 Species Characterization of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis")

 