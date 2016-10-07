#recode
#redone for filtered files; Sept28-16
#load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)

#call data and recode for DNa, RNA and phage
everyone <- read.csv("DNA_RNA_phage_viral_species_all_v2.csv", stringsAsFactors = FALSE)


#omit empty column
everyone$X <- NULL
everyone$Total_Reads <- NULL


#ref manual
vref <- read.csv("ref_ncbi_species_virome_keep_pap_types.csv", header=FALSE, stringsAsFactors = FALSE)

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
# write.csv(everyone4, "DNA_RNA_phage_viral_species_all_with_pap_types.csv")
