#assign ncbi code to species, family and genus

#call data
total <- read.csv("DNA_RNA_phage_viral_species_all_v2.csv")

#select ncbi code
viral <- total %>%
  select(Viral_Species)

taxinfo <- taxize::ncbi_get_taxon_summary(viral$Viral_Species, rank="family") #assign number to species; assign to species, family and genus and get same answer

###this will give species and will have to manually look at the unknowns
#NCBI_family.R and NCBI_genus.R were able to assign viral family and genus
#NCBI_species.R assigns species

#write taxinfo to file
# write.csv(taxinfo, "viral_ncbi_code_species.csv")

########################################################################################
#recode
#redone for filtered files; Sept28-16
#load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)

#call data and recode for DNa, RNA and phage
everyone <- read.csv("DNA_RNA_phage_viral_species_all_v2.csv", stringsAsFactors = FALSE)
vogueA <- read.csv("DNA_RNA_phage_viral_species_1A_v2.csv", stringsAsFactors = FALSE)
vogueB <- read.csv("DNA_RNA_phage_viral_species_1B_v2.csv", stringsAsFactors = FALSE)
vogue1b2 <- read.csv("DNA_RNA_phage_viral_species_1B2_v2.csv", stringsAsFactors = FALSE)

#omit empty column
everyone$X <- NULL
everyone$Total_Reads <- NULL
vogueA$X <- NULL
vogueA$Total_Reads <- NULL
vogueB$X <- NULL
vogueB$Total_Reads <- NULL
vogue1b2$X <- NULL
vogue1b2$Total_Reads <- NULL

#ref manual
vref <- read.csv("ref_ncbi_species_virome.csv", header=FALSE, stringsAsFactors = FALSE)

#omit empty rows and columns
# vref <- vref %>%
#   select(V1, V2)
# vref <- vref[c(1:72),]

#recode ncbi code to species
#recode function
recoderFunc <- function(data, oldvalue, newvalue) {
  
  # convert any factors to characters
  
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  
  # create the return vector
  
  newvec <- data
  
  # put recoded values into the correct position in the return vector
  
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  
  newvec
  
}

#recode dataframes
#everyone
everyone2 <- recoderFunc(everyone, vref$V1, vref$V2)
vogueA2 <- recoderFunc(vogueA, vref$V1, vref$V2)
vogueB2 <- recoderFunc(vogueB, vref$V1, vref$V2)
vogue1b2a <- recoderFunc(vogue1b2, vref$V1, vref$V2)

#rename column
# everyone2 <- dplyr::rename(everyone2, Virus_Type = Viral_Species)
# vogueA2 <- dplyr::rename(vogueA2, Virus_Type = Viral_Family)
# vogueB2 <- dplyr::rename(vogueB2, Virus_Type = Viral_Family)
# vogue1b2a <- dplyr::rename(vogue1b2a, Virus_Type = Viral_Family)

#make columns into integers
everyone2[, -1] <- lapply(everyone2[, -1], as.integer)
vogueA2[, -1] <- lapply(vogueA2[, -1], as.integer)
vogueB2[, -1] <- lapply(vogueB2[, -1], as.integer)
vogue1b2a[, -1] <- lapply(vogue1b2a[, -1], as.integer)

#gathering like types                    
everyone3 <- ddply(everyone2,c("Viral_Species"),numcolwise(sum)) #includes all columns
vogueA3 <- ddply(vogueA2,c("Viral_Species"),numcolwise(sum)) #includes all columns
vogueB3 <- ddply(vogueB2,c("Viral_Species"),numcolwise(sum)) #includes all columns
vogue1b2b <- ddply(vogue1b2a,c("Viral_Species"),numcolwise(sum)) #includes all columns

#na to zero
everyone3[is.na(everyone3)] <- 0
vogueA3[is.na(vogueA3)] <- 0
vogueB3[is.na(vogueB3)] <- 0
vogue1b2b[is.na(vogue1b2b)] <- 0

#remove '1'
#remove #1 (root)
everyone4 <- everyone3[!everyone3$Viral_Species == "1",]
vogueA4 <- vogueA3[!vogueA3$Viral_Species == "1",]
vogueB4 <- vogueB3[!vogueB3$Viral_Species == "1",]
vogue1b2c <- vogue1b2b[!vogue1b2b$Viral_Species == "1",]

#write to file
# write.csv(everyone4, "DNA_RNA_phage_viral_species_all_v3.csv")
# write.csv(vogueA4, "DNA_RNA_phage_viral_species_1A_v3.csv")
# write.csv(vogueB4, "DNA_RNA_phage_viral_species_1B_v3.csv")
# write.csv(vogue1b2c, "DNA_RNA_phage_viral_species_1B2_v3.csv")

########################################################
#assign ncbi code to genus
#ref manual
vref <- read.csv("ref_ncbi_genus_virome.csv", header=FALSE, stringsAsFactors = FALSE)

#recode dataframes
#everyone
everyone2 <- recoderFunc(everyone, vref$V1, vref$V2)
vogueA2 <- recoderFunc(vogueA, vref$V1, vref$V2)
vogueB2 <- recoderFunc(vogueB, vref$V1, vref$V2)
vogue1b2a <- recoderFunc(vogue1b2, vref$V1, vref$V2)
 
#rename column  
everyone2 <- dplyr::rename(everyone2, Viral_Genus = Viral_Species)
vogueA2 <- dplyr::rename(vogueA2, Viral_Genus = Viral_Species)
vogueB2 <- dplyr::rename(vogueB2, Viral_Genus = Viral_Species)
vogue1b2a <- dplyr::rename(vogue1b2a, Viral_Genus = Viral_Species)

#make columns into integers
everyone2[, -1] <- lapply(everyone2[, -1], as.integer)
vogueA2[, -1] <- lapply(vogueA2[, -1], as.integer)
vogueB2[, -1] <- lapply(vogueB2[, -1], as.integer)
vogue1b2a[, -1] <- lapply(vogue1b2a[, -1], as.integer)

#gathering like types                    
everyone3 <- ddply(everyone2,c("Viral_Genus"),numcolwise(sum)) #includes all columns
vogueA3 <- ddply(vogueA2,c("Viral_Genus"),numcolwise(sum)) #includes all columns
vogueB3 <- ddply(vogueB2,c("Viral_Genus"),numcolwise(sum)) #includes all columns
vogue1b2b <- ddply(vogue1b2a,c("Viral_Genus"),numcolwise(sum)) #includes all columns

#na to zero
everyone3[is.na(everyone3)] <- 0
vogueA3[is.na(vogueA3)] <- 0
vogueB3[is.na(vogueB3)] <- 0
vogue1b2b[is.na(vogue1b2b)] <- 0

#remove '1'
everyone4 <- everyone3[!everyone3$Viral_Genus == "1",]
vogueA4 <- vogueA3[!vogueA3$Viral_Genus == "1",]
vogueB4 <- vogueB3[!vogueB3$Viral_Genus == "1",]
vogue1b2c <- vogue1b2b[!vogue1b2b$Viral_Genus == "1",]

#write to file
# write.csv(everyone4, "DNA_RNA_phage_viral_genus_all.csv")
# write.csv(vogueA4, "DNA_RNA_phage_viral_genus_1A.csv")
# write.csv(vogueB4, "DNA_RNA_phage_viral_genus_1B.csv")
# write.csv(vogue1b2c, "DNA_RNA_phage_viral_genus_1B2.csv")

############################
#assign ncbi code to family
#ref manual
vref <- read.csv("ref_ncbi_family_virome.csv", header=FALSE, stringsAsFactors = FALSE)

#recode dataframes
#everyone
everyone2 <- recoderFunc(everyone, vref$V1, vref$V2)
vogueA2 <- recoderFunc(vogueA, vref$V1, vref$V2)
vogueB2 <- recoderFunc(vogueB, vref$V1, vref$V2)
vogue1b2a <- recoderFunc(vogue1b2, vref$V1, vref$V2)

#rename column  
everyone2 <- dplyr::rename(everyone2, Viral_Family = Viral_Species)
vogueA2 <- dplyr::rename(vogueA2, Viral_Family = Viral_Species)
vogueB2 <- dplyr::rename(vogueB2, Viral_Family = Viral_Species)
vogue1b2a <- dplyr::rename(vogue1b2a, Viral_Family = Viral_Species)

#make columns into integers
everyone2[, -1] <- lapply(everyone2[, -1], as.integer)
vogueA2[, -1] <- lapply(vogueA2[, -1], as.integer)
vogueB2[, -1] <- lapply(vogueB2[, -1], as.integer)
vogue1b2a[, -1] <- lapply(vogue1b2a[, -1], as.integer)

#gathering like types                    
everyone3 <- ddply(everyone2,c("Viral_Family"),numcolwise(sum)) #includes all columns
vogueA3 <- ddply(vogueA2,c("Viral_Family"),numcolwise(sum)) #includes all columns
vogueB3 <- ddply(vogueB2,c("Viral_Family"),numcolwise(sum)) #includes all columns
vogue1b2b <- ddply(vogue1b2a,c("Viral_Family"),numcolwise(sum)) #includes all columns

#na to zero
everyone3[is.na(everyone3)] <- 0
vogueA3[is.na(vogueA3)] <- 0
vogueB3[is.na(vogueB3)] <- 0
vogue1b2b[is.na(vogue1b2b)] <- 0

#remove '1'
everyone4 <- everyone3[!everyone3$Viral_Family == "1",]
vogueA4 <- vogueA3[!vogueA3$Viral_Family == "1",]
vogueB4 <- vogueB3[!vogueB3$Viral_Family == "1",]
vogue1b2c <- vogue1b2b[!vogue1b2b$Viral_Family == "1",]

#write to file
# write.csv(everyone4, "DNA_RNA_phage_viral_family_all.csv")
# write.csv(vogueA4, "DNA_RNA_phage_viral_family_1A.csv")
# write.csv(vogueB4, "DNA_RNA_phage_viral_family_1B.csv")
# write.csv(vogue1b2c, "DNA_RNA_phage_viral_family_1B2.csv")

#########################
#viral family to type
#call data and recode for DNa, RNA and phage
everyone <- read.csv("DNA_RNA_phage_viral_family_all.csv", stringsAsFactors = FALSE)
vogueA <- read.csv("DNA_RNA_phage_viral_family_1A.csv", stringsAsFactors = FALSE)
vogueB <- read.csv("DNA_RNA_phage_viral_family_1B.csv", stringsAsFactors = FALSE)
vogue1b2 <- read.csv("DNA_RNA_phage_viral_family_1B2.csv", stringsAsFactors = FALSE)

#omit empty column
everyone$X <- NULL
vogueA$X <- NULL
vogueB$X <- NULL
vogue1b2$X <- NULL

#ref manual
vref <- read.csv("ref_family_type.csv", header=FALSE, stringsAsFactors = FALSE)

#omit empty rows and columns
# vref <- vref %>%
#   select(V1, V2)
# vref <- vref[c(1:72),]

#recode ncbi code to species
#recode function
recoderFunc <- function(data, oldvalue, newvalue) {
  
  # convert any factors to characters
  
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  
  # create the return vector
  
  newvec <- data
  
  # put recoded values into the correct position in the return vector
  
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  
  newvec
  
}

#recode dataframes
#everyone
everyone2 <- recoderFunc(everyone, vref$V1, vref$V2)
vogueA2 <- recoderFunc(vogueA, vref$V1, vref$V2)
vogueB2 <- recoderFunc(vogueB, vref$V1, vref$V2)
vogue1b2a <- recoderFunc(vogue1b2, vref$V1, vref$V2)

#rename column
everyone2 <- dplyr::rename(everyone2, Viral_Type = Viral_Family)
vogueA2 <- dplyr::rename(vogueA2, Viral_Type = Viral_Family)
vogueB2 <- dplyr::rename(vogueB2, Viral_Type = Viral_Family)
vogue1b2a <- dplyr::rename(vogue1b2a, Viral_Type = Viral_Family)

#gathering like types                    
everyone3 <- ddply(everyone2,c("Viral_Type"),numcolwise(sum)) #includes all columns
vogueA3 <- ddply(vogueA2,c("Viral_Type"),numcolwise(sum)) #includes all columns
vogueB3 <- ddply(vogueB2,c("Viral_Type"),numcolwise(sum)) #includes all columns
vogue1b2b <- ddply(vogue1b2a,c("Viral_Type"),numcolwise(sum)) #includes all columns

#na to zero
everyone3[is.na(everyone3)] <- 0
vogueA3[is.na(vogueA3)] <- 0
vogueB3[is.na(vogueB3)] <- 0
vogue1b2b[is.na(vogue1b2b)] <- 0

#write to file
# write.csv(everyone3, "viral_type_all.csv")
# write.csv(vogueA3, "viral_type_1A.csv")
# write.csv(vogueB3, "viral_type_1B.csv")
# write.csv(vogue1b2b, "viral_type_1B2.csv")

#######
#wanted to make groupings but decided to condense family, and add types too
#viral family to type
#call data and recode for DNa, RNA and phage
everyone <- read.csv("DNA_RNA_phage_viral_family_all.csv", stringsAsFactors = FALSE)
vogueA <- read.csv("DNA_RNA_phage_viral_family_1A.csv", stringsAsFactors = FALSE)
vogueB <- read.csv("DNA_RNA_phage_viral_family_1B.csv", stringsAsFactors = FALSE)
vogue1b2 <- read.csv("DNA_RNA_phage_viral_family_1B2.csv", stringsAsFactors = FALSE)

#omit empty column
everyone$X <- NULL
vogueA$X <- NULL
vogueB$X <- NULL
vogue1b2$X <- NULL

#ref manual
vref <- read.csv("ref_familyandtype_condensed.csv", header=FALSE, stringsAsFactors = FALSE)

#omit empty rows and columns
# vref <- vref %>%
#   select(V1, V2)
# vref <- vref[c(1:72),]

#recode ncbi code to species
#recode function
recoderFunc <- function(data, oldvalue, newvalue) {
  
  # convert any factors to characters
  
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  
  # create the return vector
  
  newvec <- data
  
  # put recoded values into the correct position in the return vector
  
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  
  newvec
  
}

#recode dataframes
#everyone
everyone2 <- recoderFunc(everyone, vref$V1, vref$V2)
vogueA2 <- recoderFunc(vogueA, vref$V1, vref$V2)
vogueB2 <- recoderFunc(vogueB, vref$V1, vref$V2)
vogue1b2a <- recoderFunc(vogue1b2, vref$V1, vref$V2)

#rename column
everyone2 <- dplyr::rename(everyone2, Viruses = Viral_Family)
vogueA2 <- dplyr::rename(vogueA2, Viruses = Viral_Family)
vogueB2 <- dplyr::rename(vogueB2, Viruses = Viral_Family)
vogue1b2a <- dplyr::rename(vogue1b2a, Viruses = Viral_Family)

#gathering like types                    
everyone3 <- ddply(everyone2,c("Viruses"),numcolwise(sum)) #includes all columns
vogueA3 <- ddply(vogueA2,c("Viruses"),numcolwise(sum)) #includes all columns
vogueB3 <- ddply(vogueB2,c("Viruses"),numcolwise(sum)) #includes all columns
vogue1b2b <- ddply(vogue1b2a,c("Viruses"),numcolwise(sum)) #includes all columns

#na to zero
everyone3[is.na(everyone3)] <- 0
vogueA3[is.na(vogueA3)] <- 0
vogueB3[is.na(vogueB3)] <- 0
vogue1b2b[is.na(vogue1b2b)] <- 0

#write to file
# write.csv(everyone3, "viral_groups_all.csv")
# write.csv(vogueA3, "viral_groups_1A.csv")
# write.csv(vogueB3, "viral_groups_1B.csv")
# write.csv(vogue1b2b, "viral_groups_1B2.csv")

#####
#same data set but minus papillomaviridae

everyone4 <- everyone3[!everyone3$Viruses == "Papillomaviridae",]
vogueA4 <- vogueA3[!vogueA3$Viruses == "Papillomaviridae",]
vogueB4 <- vogueB3[!vogueB3$Viruses == "Papillomaviridae",]
vogue1b2c <- vogue1b2b[!vogue1b2b$Viruses == "Papillomaviridae",]


#write to file
# write.csv(everyone4, "viral_groups_minus_papillomaviridae_all.csv")
# write.csv(vogueA4, "viral_groups_minus_papillomaviridae_1A.csv")
# write.csv(vogueB4, "viral_groups_minus_papillomaviridae_1B.csv")
# write.csv(vogue1b2c, "viral_groups_minus_papillomaviridae_1B2.csv")

############################################################################################################################################################
#NTCs and Positive Controls assign ncbi code to species

#call data
total <- read.csv("DNA_RNA_phage_viral_species_NTCs_PosCtrl.csv")

taxinfo <- taxize::ncbi_get_taxon_summary(total$Var1, rank="family") #assign number to species; assign to species, family and genus and get same answer

#write taxinfo to file
# write.csv(taxinfo, "viral_ncbi_classification_NTCs_PosCtrl.csv")
#Now we know what is in NTCs, Adeno and Entero

#merge with "DNA_RNA_phage_species"
total2 <- dplyr::rename(total, uid = Var1)

#omit extra column
total2$X <- NULL

#merge
total3 <- join(total2, taxinfo, type="full")
#write to file
# write.csv(total3, "DNA_RNA_phage_viral_species_NTCs_PosCtrl_v2.csv")