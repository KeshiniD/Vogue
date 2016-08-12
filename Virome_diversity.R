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
data <- read.csv(file.path("1B2.csv"))


#Shannon Diversity
#just have the bacterial species
viral <- data %>%
  select(Lactobacillus.crispatus, Lactobacillus.gasseri, 
         Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, Eukaryote,
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)

H <- diversity(viral) #shannon for individuals
View(H)

#rename headers
H <- dplyr::rename(H, ShannonsDiversity = H, Participants = VALUE)

#write data to file
# write.table(H, "virome_individual_diversity.csv", sep = ",", row.names = FALSE, quote = FALSE)

###############################
#Pielou's eveness
J<- H/log(specnumber(viral)) # for individuals
View(J)

#rename headers
J <- dplyr::rename(J, ShannonsDiversity = J, Participants = VALUE)

#write data to file
# write.table(J, "virome_individual_Pielou.csv", sep = ",", row.names = FALSE, quote = FALSE)

##########################################
##Rarefraction
#call for data
#want all bacteria (in columns), no participants
data <- read.csv(file.path("1B2.csv"))

viral <- data %>%
  select(Lactobacillus.crispatus, Lactobacillus.gasseri, 
         Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, Eukaryote, 
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)

#set sample to integer (should be smaller than sample size)
#should probabaly set sample size to min(rowSums(bac))?
rarefy <- rarefy(viral, sample=min(rowSums(bac))) 

#write individual species richness into file
rarefy <- as.data.frame(rarefy)
#write.csv(rarefy, "virome_individual_richness.csv")

####################################
#Good's coverage; need to figure this out for cohort
## need entropart package

#example
data(Paracou618) 
Ns <- Paracou618.MC$Ns
Coverage(Ns)

#apply to own data
#same as above
#bac counts
viral2 <-
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

#individuals
#good's coverage estimator for individuals
viral3 <- viral2 %>% #viral2 is bacteria counts seen above with participants
  group_by(Participants) %>%
  summarise(a = Coverage(Counts, Estimator = "Turing"))

#write
# write.table(viral3, "virome_individual_Good.csv", sep = ",", row.names = FALSE, quote = FALSE)

############################
#Chao estimator
#loop for chao1-1A individuals
library(BiodiversityR)
#dataframe has participant column, and OTUs as colnames with counts

newdata <- add_rownames(newdata, "Participants")
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
#write.csv(df_list_chao, "virome_individual_chao1.csv")

#merge shannon, peilous, rarefaction, good and chao1
a <- read.csv(file.path("virome_individual_Pielou.csv"))
b <- read.csv(file.path("virome_individual_richness.csv"))
c <- read.csv(file.path("virome_individual_diversity.csv"))
d <- read.csv(file.path("virome_individual_chao1.csv"))
e <- read.csv(file.path("virome_individual_Good.csv"))
# b$X <- NULL #remove random empty column
# d <- dplyr::rename(d, Participants = X, Chao1 = V1) 
# e <- dplyr::rename(e, Participants = X, GoodsCoverageEstimator = V1) 

#merge three folders together
diversity <- cbind(a,b,c,d,e)
#remove duplicate columns
# diversity2 <- diversity[ -c(3, 5, 7, 9) ]

#write
# write.csv(diversity2, "virome_individual_all_diversity.csv")