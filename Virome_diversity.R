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
