#load packages
library(pwr)

##Power Calculations
pwr.t2n.test(n1 = 26, n2=104 , sig.level = 0.05, power = 0.8)
# t test power calculation 
#n1 = 26
#n2 = 104
#d = 0.6189457 (can detect medium differences)
#sig.level = 0.05
#power = 0.8
#alternative = two.sided

pwr.2p2n.test(n1 = 26, n2=104 , sig.level = 0.05, power = 0.8)
#difference of proportion power calculation for binomial distribution (arcsine transformation) 
#h = 0.6142887 (can detect medium differences)
#n1 = 26
#n2 = 104
#sig.level = 0.05
#power = 0.8
#alternative = two.sided
#NOTE: different sample sizes

##Diversity Indices
#load packages
library(vegan)
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)
library(entropart)
library(epitools)

#load dataset
data <- read.csv(file.path("data1A_1B2.csv"))

#column into row labels
data2 <- data[,-1]
rownames(data2) <- data[,1]

#select subset of participants wish to analyze
data2 <- data2 %>%
  select(Vogue1A.01.003, Vogue1A.01.007, Vogue1A.01.012, 
         Vogue1A.01.016, Vogue1A.01.017, Vogue1A.01.019, Vogue1A.01.020, 
         Vogue1A.01.022, Vogue1A.01.023, Vogue1A.01.024, Vogue1A.01.025, 
         Vogue1A.01.026, Vogue1A.01.029, Vogue1A.01.030, Vogue1A.01.033, 
         Vogue1A.01.034, Vogue1A.01.036, Vogue1A.01.041, Vogue1A.01.042, 
         Vogue1A.01.046, Vogue1A.01.049, Vogue1A.01.054, Vogue1A.01.060, 
         Vogue1A.01.061, Vogue1A.01.063, Vogue1A.01.066, Vogue1A.01.067, 
         Vogue1A.01.072, Vogue1A.01.073, Vogue1A.01.077, Vogue1A.01.078, 
         Vogue1A.01.079, Vogue1A.01.082, Vogue1A.01.085, Vogue1A.01.089, 
         Vogue1A.01.090, Vogue1A.01.092, Vogue1A.01.095, Vogue1A.01.097, 
         Vogue1A.01.099, Vogue1A.01.100, Vogue1A.01.107, Vogue1A.01.112, 
         Vogue1A.01.113, Vogue1A.01.114, Vogue1A.01.118, Vogue1A.01.123, 
         Vogue1A.01.125, Vogue1A.01.126, Vogue1A.01.127, Vogue1A.01.128, 
         Vogue1A.01.130, Vogue1A.01.134, Vogue1A.01.135, Vogue1A.01.137, 
         Vogue1A.01.139, Vogue1A.01.140, Vogue1A.01.142, Vogue1A.01.145, 
         Vogue1A.01.146, Vogue1A.01.147, Vogue1A.01.150, Vogue1A.01.152, 
         Vogue1A.01.156, Vogue1A.01.157, Vogue1A.01.161, Vogue1A.01.162, 
         Vogue1A.01.163, Vogue1A.01.166, Vogue1A.01.169, Vogue1A.01.170, 
         Vogue1A.01.172, Vogue1A.01.173, Vogue1A.01.174, Vogue1A.01.175, 
         Vogue1A.01.184, Vogue1A.01.186, Vogue1A.01.187, Vogue1A.01.191, 
         Vogue1A.01.192, Vogue1A.01.197, Vogue1A.01.198, Vogue1A.01.200, 
         Vogue1A.01.201, Vogue1A.01.203, Vogue1A.01.205, Vogue1A.01.207, 
         Vogue1A.01.208, Vogue1A.01.211, Vogue1A.01.212, Vogue1A.01.213, 
         Vogue1A.01.214, Vogue1A.01.215, Vogue1A.01.216, Vogue1A.01.217, 
         Vogue1A.01.218, Vogue1A.01.219, Vogue1A.01.224, Vogue1A.01.227, 
         Vogue1A.01.231, Vogue1A.01.233, Vogue1A.01.236, Vogue1A.01.237, 
         Vogue1A.01.238)

#transpose dataset
data2 <- as.data.frame(t(data2))


##Shannon's Diversity for Individuals
H <- diversity(data2) 
H <- as.data.frame(H)
View(H)

#row names into column
H <- add_rownames(H, "VALUE")
#rename headers
H <- dplyr::rename(H, ShannonsDiversity = H, Participants = VALUE)

#write data to file 
write.table(H, "1A_subset_individual_diversity.csv", sep = ",", row.names = FALSE, quote = FALSE)
div <- read.csv(file.path("1A_subset_individual_diversity.csv"))

#SD cohort
#first need to make new table with total counts for each bacterial species
#load dataset
data <- read.csv(file.path("data1A_1B2.csv"))

#column into row labels
data2 <- data[,-1]
rownames(data2) <- data[,1]

#select subset of participants wish to analyze
data2 <- data2 %>%
  select(Vogue1A.01.003, Vogue1A.01.007, Vogue1A.01.012, 
         Vogue1A.01.016, Vogue1A.01.017, Vogue1A.01.019, Vogue1A.01.020, 
         Vogue1A.01.022, Vogue1A.01.023, Vogue1A.01.024, Vogue1A.01.025, 
         Vogue1A.01.026, Vogue1A.01.029, Vogue1A.01.030, Vogue1A.01.033, 
         Vogue1A.01.034, Vogue1A.01.036, Vogue1A.01.041, Vogue1A.01.042, 
         Vogue1A.01.046, Vogue1A.01.049, Vogue1A.01.054, Vogue1A.01.060, 
         Vogue1A.01.061, Vogue1A.01.063, Vogue1A.01.066, Vogue1A.01.067, 
         Vogue1A.01.072, Vogue1A.01.073, Vogue1A.01.077, Vogue1A.01.078, 
         Vogue1A.01.079, Vogue1A.01.082, Vogue1A.01.085, Vogue1A.01.089, 
         Vogue1A.01.090, Vogue1A.01.092, Vogue1A.01.095, Vogue1A.01.097, 
         Vogue1A.01.099, Vogue1A.01.100, Vogue1A.01.107, Vogue1A.01.112, 
         Vogue1A.01.113, Vogue1A.01.114, Vogue1A.01.118, Vogue1A.01.123, 
         Vogue1A.01.125, Vogue1A.01.126, Vogue1A.01.127, Vogue1A.01.128, 
         Vogue1A.01.130, Vogue1A.01.134, Vogue1A.01.135, Vogue1A.01.137, 
         Vogue1A.01.139, Vogue1A.01.140, Vogue1A.01.142, Vogue1A.01.145, 
         Vogue1A.01.146, Vogue1A.01.147, Vogue1A.01.150, Vogue1A.01.152, 
         Vogue1A.01.156, Vogue1A.01.157, Vogue1A.01.161, Vogue1A.01.162, 
         Vogue1A.01.163, Vogue1A.01.166, Vogue1A.01.169, Vogue1A.01.170, 
         Vogue1A.01.172, Vogue1A.01.173, Vogue1A.01.174, Vogue1A.01.175, 
         Vogue1A.01.184, Vogue1A.01.186, Vogue1A.01.187, Vogue1A.01.191, 
         Vogue1A.01.192, Vogue1A.01.197, Vogue1A.01.198, Vogue1A.01.200, 
         Vogue1A.01.201, Vogue1A.01.203, Vogue1A.01.205, Vogue1A.01.207, 
         Vogue1A.01.208, Vogue1A.01.211, Vogue1A.01.212, Vogue1A.01.213, 
         Vogue1A.01.214, Vogue1A.01.215, Vogue1A.01.216, Vogue1A.01.217, 
         Vogue1A.01.218, Vogue1A.01.219, Vogue1A.01.224, Vogue1A.01.227, 
         Vogue1A.01.231, Vogue1A.01.233, Vogue1A.01.236, Vogue1A.01.237, 
         Vogue1A.01.238)

#transpose dataset
data2 <- as.data.frame(t(data2))
#row into column
data2 <- add_rownames(data2, "Participants")

#write data to file 
write.table(data2, "1A_temp.csv", sep = ",", row.names = FALSE, quote = FALSE)
data2 <- read.csv(file.path("1A_temp.csv"))


#bac counts
data3 <-
  gather(data2, key = 'Bacteria', value = 'Counts', Actinobacteria.sp., 
         Alloscardovia.omnicolens, Atopobium.vaginae, 
         Bifidobacterium.breve, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Enterococcus.rattus, 
         Escherichia.coli, Eukaryote, Gardnerella.vaginalis.Group.A, 
         Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.C, 
         Gardnerella.vaginalis.Group.D, Klebsiella.pneumoniae, 
         Lactobacillus.crispatus, Lactobacillus.gasseri, 
         Lactobacillus.iners, Lactobacillus.jensenii, 
         Megasphaera.sp..genomosp..type.1, Other.Actinobacteria, 
         Other.Bacteria, Other.Bacteroidetes, Other.Bifidobacterium, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus, 
         Porphyromonas.uenonis, Prevotella.amnii, Prevotella.timonensis, 
         Pseudomonas.putida, Streptococcus.devriesei, Variovorax.paradoxus)

#then use in diversity
H2 <- data3 %>% 
  select (Bacteria, Counts) %>% #WORKED!!!! for cohort
  group_by(Bacteria) %>%
  summarize(TotalCounts = sum(Counts)) %>%
  select (TotalCounts) #stay in for diversity,can remove to see bac
F2 <- diversity(H2)
View(F2)

#remove the absent bacterial species and see if this alters SD
H2 <- H2[-c(1,8,14), ]
F3 <- diversity(H2)
View(F3) #exactly the same
F3 <- as.data.frame(F3) #data.frame

#rename headers
F3 <- dplyr::rename(F3, ShannonsDiversity = F3)

#write data to file 
write.table(F3, "1A_subset_cohort_diversity.csv", sep = ",", row.names = FALSE, quote = FALSE)
cdiv <- read.csv(file.path("1A_subset_cohort_diversity.csv"))


##Pielou's eveness
#remove participants
data2 <- data2[-1 ]
J<- H/log(specnumber(data2)) # for individuals
J <- as.data.frame(J)
#row into column
J <- add_rownames(J, "Participants")
#rename headers
#rename headers
J <- dplyr::rename(J, PielousEveness = H)
View(J)

#write data to file (altered headings and called back)
write.table(J, "1A_subset_individual_Pielou.csv", sep = ",", row.names = FALSE, quote = FALSE)
Piel <- read.csv(file.path("1A_subset_individual_Pielou.csv"))

J2 <- F2/log(specnumber(H2, MARGIN = 2)) #not working for entire cohort
#NOV-12-2015: fixed above code; margin set to 2; finds frequencies of species
#specnumber want number of species and we can manually enter 21 to get J2
#fixed it with below code, and can be used for diversity
J2 <- as.data.frame(J2)
J2 <- dplyr::rename(J2, PielousEveness = J2)
View(J2)

#NOV-12-2015: do not need below code
# want to sum up all bacteria in one row, with bacteria as columns
# can then use this in specnumber() and then pielou for cohort
h4 <- colSums(H2)
H5 <-  t(h4)
J2 <- F2/log(H5) 
View(J2)

#write data to file (altered headings and called back)
write.table(J2, "1A_subset_cohort_Pielou.csv", sep = ",", row.names = FALSE, quote = FALSE)
cPiel <- read.csv(file.path("1A_subset_cohort_Pielou.csv"))
