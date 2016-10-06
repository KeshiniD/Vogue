#Virome Diversity 

library(vegan)
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)
library(entropart)
library(epitools)
library(fossil)
library(BiodiversityR)

#call for data
data <- read.csv(file.path("DNA_RNA_phage_viral_family_all_v2.csv"))
data$X.1 <- NULL
data$X <- NULL

#move species into headers, and participants into rows
rownames(data) <- data[,1]
data[,1] <- NULL
data2 <- as.data.frame(t(data))

#Shannon Diversity
#just have the species
H <- diversity(data2) #shannon for individuals
H <- data.frame(H)
View(H)

#rename headers
H <- dplyr::rename(H, ShannonsDiversity = H)

#write data to file
# write.table(H, "virome_individual_SD_all.csv", sep = ",", row.names = FALSE, quote = FALSE)

###############################
#Pielou's eveness
J<- H/log(specnumber(data2)) # for individuals
View(J)

#rename headers
J <- dplyr::rename(J, PielousEvenness = ShannonsDiversity)

#write data to file
# write.table(J, "virome_individual_Pielou_all.csv", sep = ",", row.names = FALSE, quote = FALSE)

##########################################
##Rarefraction
#call for data
#want all bacteria (in columns), no participants
# data <- read.csv(file.path("1B2.csv"))
# 
# viral <- data %>%
#   select(Lactobacillus.crispatus, Lactobacillus.gasseri, 
#          Lactobacillus.iners, Lactobacillus.jensenii, 
#          Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
#          Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
#          Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
#          Clostridium.genomosp..BVAB3, Escherichia.coli, Eukaryote, 
#          Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
#          Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
#          Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
#          Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
#          Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)
# 
# #set sample to integer (should be smaller than sample size)
# #should probabaly set sample size to min(rowSums(bac))?
# rarefy <- rarefy(viral, sample=min(rowSums(bac))) 
# 
# #write individual species richness into file
# rarefy <- as.data.frame(rarefy)
# #write.csv(rarefy, "virome_individual_richness.csv")

####################################
#Good's coverage; need to figure this out for cohort
## need entropart package

#example
data(Paracou618) 
Ns <- Paracou618.MC$Ns
Coverage(Ns)

#apply to own data
#same as above
#virus counts
#went into data2, and manually fixed virus names to exlcude spaces
#but it is all fixed now and do not need to worry
data3 <-
  gather(data2, key = 'Virus', value = 'Counts', Adenoviridae, Alloherpesviridae,
         Alphaflexiviridae, Ampullaviridae, Anelloviridae, Arteriviridae, 
         Ascoviridae, Astroviridae, Baculoviridae, Bicaudaviridae, Caliciviridae, 
         Caudovirales, Caulimoviridae, Chrysoviridae, Circoviridae, Coronaviridae, 
         Endornaviridae, Flaviviridae, Fuselloviridae, Hepadnaviridae, Hepeviridae,
         Herpesviridae, Hytrosaviridae, Iflaviridae, Inoviridae, Iridoviridae, 
         Lipothrixviridae, Luteoviridae, Marseilleviridae, Microviridae, 
         Mimiviridae, Myoviridae, Nimaviridae, Nudiviridae, Other_dsDNA_viruses, 
         Other_Phages, Other_ssDNA_viruses, Other_ssRNA_negative_strand_viruses, 
         Other_ssRNA_positive_strand_viruses, Other_Viruses, Papillomaviridae, 
         Parvoviridae, Phycodnaviridae, Picornaviridae, Podoviridae, 
         Polydnaviridae, Polyomaviridae, Potyviridae, Poxviridae, Reoviridae, 
         Retroviridae, Secoviridae, Siphoviridae, Tombusviridae, Totiviridae, 
         Turriviridae,  Tymoviridae, Virgaviridae)
#rename
data3 <- dplyr::rename(data3, Participants = X)


#individuals
#good's coverage estimator for individuals
data4 <- data3 %>% #data3 is virus counts seen above with participants
  group_by(Participants) %>%
  summarise(a = Coverage(Counts, Estimator = "Turing"))

#rename
data4 <- dplyr::rename(data4, GoodsCoverage = a)

#write
# write.table(data4, "virome_individual_Good_all.csv", sep = ",", row.names = FALSE, quote = FALSE)

############################
#Chao estimator
#loop for chao1-1A individuals
library(BiodiversityR)
#dataframe has participant column, and OTUs as colnames with counts
#data2 from above
# newdata <- add_rownames(newdata, "Participants")
Participant <- newdata$Participants

df <- data.frame(var = c(), chao = c())

df_list_chao <- lapply(Participant, function(Participants)  { 
  
  subset_data <- newdata[ which(newdata$Participants==Participants), ]
  d <- diversityresult(subset_data, index = 'chao') 
  
  chao <- d$chao 
  
  row <- data.frame(var = Participants, chao = chao)
  row
})  
df_list_chao <- do.call(rbind, df_list_chao)

#write to file
# write.csv(df_list_chao, "virome_individual_chao1_all.csv")

#merge shannon, peilous, rarefaction, good and chao1
a <- read.csv(file.path("virome_individual_Pielou_all.csv"))
# b <- read.csv(file.path("virome_individual_richness.csv"))
c <- read.csv(file.path("virome_individual_SD_all.csv"))
d <- read.csv(file.path("virome_individual_chao1_all.csv"))
e <- read.csv(file.path("virome_individual_Good_all.csv"))
# b$X <- NULL #remove random empty column
d$X <- NULL
d <- dplyr::rename(d, Participants = var, Chao1 = chao)
# e <- dplyr::rename(e, Participants = X, GoodsCoverageEstimator = V1) 

#merge three folders together
diversity <- cbind(a,c,d,e)
#remove duplicate columns
# diversity2 <- diversity[ -c(5) ]

#write
# write.csv(diversity2, "virome_individual_diversity_all.csv")

###################################################################################
###################################################################################
#diversity statistics for 3 cohorts separately; as whole
#call for data
vogueA <- read.csv("DNA_RNA_phage_viral_family_1A_v2.csv")
vogueB <- read.csv("DNA_RNA_phage_viral_family_1B_v2.csv")
vogue1B2 <- read.csv("DNA_RNA_phage_viral_family_1B2_v2.csv")

#remove extra column
vogueA$X <- NULL
vogueB$X <- NULL
vogue1B2$X <- NULL

#Na to zero
vogueA[is.na(vogueA)] <- 0
vogueB[is.na(vogueB)] <- 0
vogue1B2[is.na(vogue1B2)] <- 0

################
#ShannonDiversity
#cohort

#1A
#move species into headers, and participants into rows
rownames(vogueA) <- vogueA[,1]
vogueA[,1] <- NULL
vogueA2 <- as.data.frame(t(vogueA))

#first need to make new table with total counts for each bacterial species
#bac counts
vogueA3 <-
  gather(vogueA2, key = 'Virus', value = 'Counts', Adenoviridae, Alloherpesviridae,
         Alphaflexiviridae, Anelloviridae, Ascoviridae, Baculoviridae, 
         Bicaudaviridae, Caudovirales, Circoviridae, Coronaviridae, 
         Endornaviridae, Flaviviridae, Fuselloviridae, Hepadnaviridae, 
         Herpesviridae, Hytrosaviridae, Iridoviridae, 
         Lipothrixviridae, Luteoviridae, Marseilleviridae, Microviridae, 
         Mimiviridae, Myoviridae, Nimaviridae, Nudiviridae, Other_dsDNA_viruses, 
         Other_Phages, Other_ssDNA_viruses,  
         Other_ssRNA_positive_strand_viruses, Other_Viruses, Papillomaviridae, 
         Parvoviridae, Phycodnaviridae, Picornaviridae, Podoviridae, 
         Polydnaviridae, Polyomaviridae, Poxviridae, Reoviridae, 
         Retroviridae, Siphoviridae, Tombusviridae, Tymoviridae, Virgaviridae)

#then use in diversity
H2 <- vogueA3 %>% 
  select (Virus, Counts) %>% #WORKED!!!! for cohort
  group_by(Virus) %>%
  summarize(TotalCounts = sum(Counts)) %>%
  select (TotalCounts) #stay in for diversity,can remove to see bac
F2 <- diversity(H2)
F2 <- as.data.frame(F2)
View(F2)

#headers
F2 <- dplyr::rename(F2, Vogue1A = F2) #column header

#############
#1B
#move species into headers, and participants into rows
rownames(vogueB) <- vogueB[,1]
vogueB[,1] <- NULL
vogueB2 <- as.data.frame(t(vogueB))

#first need to make new table with total counts for each bacterial species
#bac counts
vogueB3 <-
  gather(vogueB2, key = 'Virus', value = 'Counts', Adenoviridae, Alloherpesviridae,
         Alphaflexiviridae, Ampullaviridae, Anelloviridae,  
         Ascoviridae, Astroviridae, Baculoviridae, Bicaudaviridae,  
         Caudovirales, Caulimoviridae, Chrysoviridae, Coronaviridae, 
         Endornaviridae, Flaviviridae, Fuselloviridae, Hepeviridae,
         Herpesviridae, Hytrosaviridae, Iflaviridae, Inoviridae, Iridoviridae, 
         Lipothrixviridae, Luteoviridae, Marseilleviridae,  
         Mimiviridae, Myoviridae, Nimaviridae, Nudiviridae, Other_dsDNA_viruses, 
         Other_Phages, Other_ssDNA_viruses, Other_ssRNA_negative_strand_viruses, 
         Other_ssRNA_positive_strand_viruses, Other_Viruses, Papillomaviridae, 
         Phycodnaviridae, Picornaviridae, Podoviridae, 
         Polydnaviridae, Polyomaviridae, Potyviridae, Poxviridae, Reoviridae, 
         Retroviridae, Secoviridae, Siphoviridae, Totiviridae, 
         Turriviridae,  Tymoviridae, Virgaviridae)

#then use in diversity
H2 <- vogueB3 %>% 
  select (Virus, Counts) %>% #WORKED!!!! for cohort
  group_by(Virus) %>%
  summarize(TotalCounts = sum(Counts)) %>%
  select (TotalCounts) #stay in for diversity,can remove to see bac
B <- diversity(H2)
B <- as.data.frame(B)
View(B)

#headers
B <- dplyr::rename(B, Vogue1B = B) #column header

####################
#1B2
#move species into headers, and participants into rows
rownames(vogue1B2) <- vogue1B2[,1]
vogue1B2[,1] <- NULL
vogue1B2b <- as.data.frame(t(vogue1B2))

#first need to make new table with total counts for each bacterial species
#bac counts
vogue1B2c <-
  gather(vogue1B2b, key = 'Virus', value = 'Counts', Adenoviridae, 
         Alloherpesviridae, Alphaflexiviridae, Anelloviridae, Arteriviridae, 
         Ascoviridae, Baculoviridae, Bicaudaviridae, Caliciviridae, 
         Caudovirales, Chrysoviridae, Circoviridae, Flaviviridae, 
         Herpesviridae, Iridoviridae, Lipothrixviridae, Luteoviridae,  
         Mimiviridae, Myoviridae, Nimaviridae, Nudiviridae, Other_dsDNA_viruses, 
         Other_Phages, Other_Viruses, Papillomaviridae, 
         Parvoviridae, Phycodnaviridae, Picornaviridae, Podoviridae, 
         Polydnaviridae, Polyomaviridae, Poxviridae, Reoviridae, 
         Retroviridae, Siphoviridae)

#then use in diversity
H2 <- vogue1B2c %>% 
  select (Virus, Counts) %>% #WORKED!!!! for cohort
  group_by(Virus) %>%
  summarize(TotalCounts = sum(Counts)) %>%
  select (TotalCounts) #stay in for diversity,can remove to see bac
B2 <- diversity(H2)
B2 <- as.data.frame(B2)
View(B2)

#headers
B2 <- dplyr::rename(B2, Vogue1B2 = B2) #column header

#################################
#join three cohorts together
total <- cbind(F2,B,B2)

#write data to file (altered headings and called back)
# write.table(total, "cohort_shannonsdiversity.csv", sep = ",", row.names = FALSE, quote = FALSE)

##################################################################################
#Pielous cohort
# want to sum up all bacteria in one row, with bacteria as columns
# can then use this in specnumber() and then pielou for cohort
J2 <- F2/log(specnumber(H2, MARGIN = 2)) #not working for entire cohort
#NOV-12-2015: fixed above code; margin set to 2; finds frequencies of species
#specnumber want number of species and we can manually enter 21 to get J2
#fixed it with below code, and can be used for diversity
View(J2)

#1A
#variables taken from above
vogueA_P <- F2/log(specnumber(H2, MARGIN = 2))

#1B
vogueB_P <- B/log(specnumber(H2, MARGIN = 2))

#1B2
vogueB2_P <- B2/log(specnumber(H2, MARGIN = 2))

########
#join three cohorts together
total <- cbind(vogueA_P, vogueB_P, vogueB2_P)

#write data to file (altered headings and called back)
# write.table(total, "cohort_pielous.csv", sep = ",", row.names = FALSE, quote = FALSE)

###################################################################################
###################################################################################
#Goods Coverage cohort
#1A
#variables taken from above
#total counts
# H2 <- data2 %>% 
#   select (Bacteria, Counts) %>% 
#   group_by(Bacteria) %>%
#   summarize(TotalCounts = sum(Counts)) %>%
#   select (TotalCounts)

A <- H2$TotalCounts
A2 <- Coverage(A, Estimator = "Turing") #Ns has to be numeric vector
A2 <- as.data.frame(A2)

#1B
B <- H2$TotalCounts
B2 <- Coverage(B, Estimator = "Turing") #Ns has to be numeric vector
B2 <- as.data.frame(B2)

#1B2
B2a <- H2$TotalCounts
B2b <- Coverage(B2a, Estimator = "Turing") #Ns has to be numeric vector
B2b <- as.data.frame(B2b)

########
#join three cohorts together
total <- cbind(A2, B2, B2b)

#write data to file (altered headings and called back)
# write.table(total, "cohort_goodcoverage.csv", sep = ",", row.names = FALSE, quote = FALSE)

###################################################################################
###################################################################################
#Chao1 cohort
#call for data
vogueA <- read.csv("DNA_RNA_phage_viral_family_1A_v2.csv")
vogueB <- read.csv("DNA_RNA_phage_viral_family_1B_v2.csv")
vogue1B2 <- read.csv("DNA_RNA_phage_viral_family_1B2_v2.csv")

#remove extra column
vogueA$X <- NULL
vogueB$X <- NULL
vogue1B2$X <- NULL

#Na to zero
vogueA[is.na(vogueA)] <- 0
vogueB[is.na(vogueB)] <- 0
vogue1B2[is.na(vogue1B2)] <- 0

########
#1A
#format such that columns are viruses, and row names are participants
rownames(vogueA) <- vogueA[,1]
vogueA[,1] <- NULL
vogueA <- as.data.frame(t(vogueA))

#chao
A <- diversityresult(vogueA, index = 'chao')
A <- dplyr::rename(A, Vogue1A = chao)
View(A)

##########
#1B
rownames(vogueB) <- vogueB[,1]
vogueB[,1] <- NULL
vogueB <- as.data.frame(t(vogueB))

#chao
B <- diversityresult(vogueB, index = 'chao')
B <- dplyr::rename(B, Vogue1B = chao)
View(B)

############
#1B2
rownames(vogue1B2) <- vogue1B2[,1]
vogue1B2[,1] <- NULL
vogue1B2 <- as.data.frame(t(vogue1B2))

#chao
B2 <- diversityresult(vogue1B2, index = 'chao')
B2 <- dplyr::rename(B2, Vogue1B2 = chao)
View(B2)

########
#join three cohorts together
total <- cbind(A, B, B2)

#write data to file (altered headings and called back)
# write.table(total, "cohort_chao.csv", sep = ",", row.names = FALSE, quote = FALSE)

###################################################################################
#merge shannon, peilous, good and chao1
a <- read.csv(file.path("cohort_shannonsdiversity.csv"))
b <- read.csv(file.path("cohort_pielous.csv"))
c <- read.csv(file.path("cohort_goodcoverage.csv"))
d <- read.csv(file.path("cohort_chao.csv"))

#rename header
c <- dplyr::rename(c, Vogue1A = A2, Vogue1B = B2, Vogue1B2 = B2b)

#merge three folders together
diversity <- join(a,b, type="full")
diversity <- join(diversity, c, type="full")
diversity <- join(diversity, d, type="full")

#write
# write.csv(diversity, "virome_cohort_diversity.csv")
