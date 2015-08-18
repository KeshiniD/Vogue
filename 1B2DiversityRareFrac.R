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
