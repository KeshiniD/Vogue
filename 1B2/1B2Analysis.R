data <- read.csv(file.path("1B2data_groups.csv"))

#change headers
data <- dplyr::rename(data, Species = Group, Vogue1B2.01.26 = X1B2_01_26, 
                      Vogue1B2.01.35 = X1B2_01_35, Vogue1B2.01.37 = X1B2_01_37, 
                      Vogue1B2.01.38 = X1B2_01_38, Vogue1B2.01.50 = X1B2_01_50, 
                      Vogue1B2.01.52 = X1B2_01_52, Vogue1B2.01.56 = X1B2_01_56, 
                      Vogue1B2.01.58 = X1B2_01_58, Vogue1B2.01.61 = X1B2_01_61, 
                      Vogue1B2.01.62 = X1B2_01_62, Vogue1B2.01.63 = X1B2_01_63, 
                      Vogue1B2.01.64 = X1B2_01_64)
data2 <- data %>%
  select(Species, Vogue1B2.01.01, Vogue1B2.01.06, Vogue1B2.01.07, Vogue1B2.01.08, 
         Vogue1B2.01.09, Vogue1B2.01.10, Vogue1B2.01.11, Vogue1B2.01.12, 
         Vogue1B2.01.15, Vogue1B2.01.19, Vogue1B2.01.21, Vogue1B2.01.23, 
         Vogue1B2.01.26, Vogue1B2.01.28, Vogue1B2.01.29, Vogue1B2.01.35, 
         Vogue1B2.01.37, Vogue1B2.01.38, Vogue1B2.01.50, Vogue1B2.01.52, 
         Vogue1B2.01.56, Vogue1B2.01.58, Vogue1B2.01.61, Vogue1B2.01.62, 
         Vogue1B2.01.63, Vogue1B2.01.64)

#transpose data and keep as data frame, swap rows and cols
rownames(data2) <- data2[,1]
data2[,1] <- NULL

data3 <- as.data.frame(t(data2))
write.table(data3, "1B2.csv", sep = ",", row.names = FALSE, quote = FALSE)
data <- read.csv(file.path("1B2.csv"))

#bac counts
data4 <-
  gather(data, key = 'Bacteria', value = 'Counts', Lactobacillus.crispatus, 
         Lactobacillus.gasseri, Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, 
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)

vmb2 <- tbl_df(data4) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Bacteria, Counts) %>%
  mutate(Species.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#bar plot with custom colors
jColors <- c('blue', 'deepskyblue3', 'cornflowerblue', 'deepskyblue', 'green', 
             'forestgreen', 'palegreen', 'green3', 'darkgoldenrod1', 
             'purple', 'mediumorchid2', 'plum', 'firebrick', 'firebrick1', 
             'gray33', 'gray', 'mediumvioletred', 'black', 'olivedrab2', 
             'orange3', 'tomato', 'lightsalmon', 'slateblue', 'turquoise', 
             'lavender', 'rosybrown2', 'deeppink')

ggplot(data = vmb2, aes(x = Participants, y = Species.Percentage, fill = Bacteria)) + 
  geom_bar(stat = "identity") + coord_flip() + ylab("Species Proportion") +
  scale_fill_manual(values=jColors) + 
  ggtitle("Cpn60 Species Characterization of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis")

colors()


#merge bac with metadata
metadata <- read.delim(file.path("completemetadataR.txt"))
metadata <-  metadata[c(1:26), ]
metadata <- dplyr::rename(metadata, Participants = X)
total<-join(metadata, data, type="full")

write.table(total, "1B2metabac.csv", sep = ",", row.names = FALSE, quote = FALSE)

##REDO EVERYTHING TO ORGANIZE BARPLOT VIA NUGENT SCORES
#bac counts
data4 <-
  gather(total, key = 'Bacteria', value = 'Counts', Lactobacillus.crispatus, 
         Lactobacillus.gasseri, Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, 
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)


vmb2 <- tbl_df(data4) %>%
  group_by(Participants) %>%
  select(Participants, Bacteria, Counts, Nugent.score) %>%
  mutate(Species.Percentage = Counts/(sum(Counts))*100) %>%
  arrange(Participants)# can either have % or decimal
 

#bar plot with custom colors
jColors <- c('blue', 'deepskyblue3', 'cornflowerblue', 'deepskyblue', 'green', 
             'palegreen', 'forestgreen', 'green3', 'darkgoldenrod1', 
             'purple', 'mediumorchid2', 'plum', 'firebrick', 'firebrick1', 
             'gray33', 'gray', 'mediumvioletred', 'black', 'olivedrab2', 
             'orange3', 'tomato', 'lightsalmon', 'slateblue', 'turquoise', 
             'lavender', 'rosybrown2', 'deeppink')

#trying to plot via nugent score
ggplot(data = vmb2, aes(x = Participants, y = Species.Percentage, fill = Bacteria)) + 
  geom_bar(stat = "identity") + coord_flip() + ylab("Species Proportion") +  scale_fill_manual(values=jColors) + 
  ggtitle("Cpn60 Species Characterization of the Vaginal Microbiome of Women with Recurrent Bacterial Vaginosis") 

#Shannon Diversity
library(vegan)

# just have the bacterial species
vmb4 <- total %>%
  select(Lactobacillus.crispatus, Lactobacillus.gasseri, 
         Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, 
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)

H <- diversity(vmb4) #shannon for individuals
View(H)

# Shannon for entire cohort (wrong)
H2 <- diversity(H)
View(H2)

#with dplyr
H2 <- total %>% #individuals
  select(Lactobacillus.crispatus, Lactobacillus.gasseri, 
         Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, 
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus) 
F <- diversity(H2)
View(F)
F2 <- dplyr::rename(F, ShannonDiveristy = X)#need to set X

#cohort
#first need to make new table with total counts for each bacterial species
H3 <- vmb2 %>% 
  select (Bacteria, Counts) %>% #WORKED!!!! for cohort
  group_by(Bacteria) %>%
  summarize(TotalCounts = sum(Counts)) %>%
  select (TotalCounts) #stay in for diversity,can remove to see bac
F3 <- diversity(H3)
View(F3)

#Pielou's eveness
J <- F/log(specnumber(H2)) # for individuals
View(J)

J2 <- F3/log(specnumber(H3, MARGIN = 1)) #not working for entire cohort
#specnumber want number of species and we can manually enter 21 to get J2
#fixed it with below code, and can be used for diversity
View(J2)

# want to sum up all bacteria in one row, with bacteria as columns
# can then use this in specnumber() and then pielou for cohort
h4 <- colSums(H2)
H5 <-  t(h4)

#rarefraction curves
H2 <- data %>% #same code as above, need in this format
  select(Lactobacillus.crispatus, Lactobacillus.iners, Lactobacillus.gasseri, 
         Lactobacillus.jensenii, Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.A, 
         Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.D, 
         Megasphaera.sp.genomosp.type.1, Escherichia.coli, Prevotella.timonensis, Clostridia.sp.BVAB2, 
         Clostridium.genomosp.BVAB3, Atopobium.vaginae, Anaerobes, Other.Clostridia, 
         Other.Bacteroidetes, Other.Proteobacteria, Other.Actinobacteria, Other.Firmicutes, Other) 
