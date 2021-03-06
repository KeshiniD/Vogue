#trying to organize bac profiles via CST
#call for data
data <- read.csv(file.path("1B2.csv"))

#add new column with CST.participants
data[,"Vogue.Participants"]  <- c('CST.II.Vogue1B2.01.01', 'CST.IVA.Vogue1B2.01.06', 
  'CST.III.Vogue1B2.01.07', 'CST.III.Vogue1B2.01.08', 'CST.III.Vogue1B2.01.09', 
  'CST.IVD.Vogue1B2.01.10', 'CST.IVC.Vogue1B2.01.11', 'CST.I.Vogue1B2.01.12', 
  'CST.I.Vogue1B2.01.15', 'CST.IVA.Vogue1B2.01.19', 'CST.III.Vogue1B2.01.21', 
  'CST.III.Vogue1B2.01.23', 'CST.IVC.Vogue1B2.01.26', 'CST.I.Vogue1B2.01.28', 
  'CST.IVC.Vogue1B2.01.29', 'CST.IVC.Vogue1B2.01.35', 'CST.IVC.Vogue1B2.01.37', 
  'CST.IVA.Vogue1B2.01.38', 'CST.III.Vogue1B2.01.50', 'CST.IVA.Vogue1B2.01.52', 
  'CST.IVA.Vogue1B2.01.56', 'CST.I.Vogue1B2.01.58', 'CST.IVD.Vogue1B2.01.61', 
  'CST.IVC.Vogue1B2.01.62', 'CST.I.Vogue1B2.01.63', 'CST.IVA.Vogue1B2.01.64')
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
jColors <- c('blue', 'deepskyblue3', 'cornflowerblue', 'deepskyblue', 'green3', 
             'forestgreen', 'palegreen', 'green', 'darkgoldenrod1', 
             'purple', 'mediumorchid2', 'plum', 'firebrick', 'yellow', 'firebrick1', 
             'gray33', 'gray', 'mediumvioletred', 'black', 'olivedrab2', 
             'orange3', 'tomato', 'lightsalmon', 'slateblue', 'turquoise', 
             'lavender', 'rosybrown2', 'deeppink')

ggplot(data = vmb, aes(x = Vogue.Participants, y = Species.Percentage, fill = Bacteria)) + 
  geom_bar(stat = "identity") + coord_flip() + ylab("Species Proportion") +
  scale_fill_manual(values=jColors) + 
  ggtitle("Cpn60 Species Characterization of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis")


########################################################################
#Mar-5-16
#order this based on CST with Dean's code
#add new column with CST.participants
data[,"CST"]  <- c('II', 'IVA', 'III', 'III', 'III', 'IVD', 'IVC', 'I', 
                  'I', 'IVA', 'III', 'III', 'IVC', 'I', 'IVC', 'IVC', 'IVC', 
                  'IVA', 'III', 'IVA', 'IVA', 'I', 'IVD', 'IVC', 'I', 'IVA')

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Bacteria, Counts, CST) %>%
  mutate(Species.Percentage = Counts/(sum(Counts))*100) %>% 
  # can either have % or decimal
  arrange(Participants)

#Dean added to order by factor levels
vmb$Participants <- factor(vmb$Participants, 
                            levels = vmb$Participants[order(vmb$CST)])

#custom colours
jColors <- c('darkgoldenrod1', 'purple', 'mediumorchid2', 'plum', 
             'firebrick', 'yellow', 'green3', 'forestgreen', 'palegreen', 
             'green' , 'firebrick1', 'blue', 'deepskyblue3', 
             'cornflowerblue', 'deepskyblue' , 'gray33', 'gray', 
             'mediumvioletred', 'black', 'olivedrab2', 'orange3', 'tomato', 
             'lightsalmon', 'slateblue', 'turquoise', 'lavender', 
             'rosybrown2', 'deeppink')

ggplot(data = vmb, aes(x = Participants, y = Species.Percentage, fill = Bacteria)) + 
  geom_bar(stat = "identity") + coord_flip() + ylab("Species Proportion") +
  scale_fill_manual(values=jColors) +  
  theme(legend.position = "bottom", legend.key.size = unit(.35, "cm"), 
        legend.title = element_blank(), plot.title = element_text(size=18), 
        axis.text = element_text(size=14), axis.title = element_text(size=18), 
        legend.text = element_text(size=11)) +
  ggtitle("Cpn60 Species Characterization of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis")
