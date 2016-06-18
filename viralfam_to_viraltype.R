#load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)

#call data and recode for DNa, RNA and phage
everyone <- read.csv("DNA_RNA_phage_viral_families_all.csv", stringsAsFactors = FALSE)
vogueA <- read.csv("DNA_RNA_phage_viral_families_1A.csv", stringsAsFactors = FALSE)
vogueB <- read.csv("DNA_RNA_phage_viral_families_1B.csv", stringsAsFactors = FALSE)
vogue1b2 <- read.csv("DNA_RNA_phage_viral_families_1B2.csv", stringsAsFactors = FALSE)

#omit empty column
everyone$X <- NULL
vogueA$X <- NULL
vogueB$X <- NULL
vogue1b2$X <- NULL

#ref manual
vref <- read.csv("virome_DNA_RNA_Phage_ref.csv", header=FALSE, stringsAsFactors = FALSE)

#omit empty rows and columns
vref <- vref %>%
  select(V1, V2)
vref <- vref[c(1:72),]

#recode families to virus types
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
everyone2 <- dplyr::rename(everyone2, Virus_Type = Viral_Families)
vogueA2 <- dplyr::rename(vogueA2, Virus_Type = Viral_Families)
vogueB2 <- dplyr::rename(vogueB2, Virus_Type = Viral_Families)
vogue1b2a <- dplyr::rename(vogue1b2a, Virus_Type = Viral_Families)

#gathering like types                    
everyone3 <- ddply(everyone2,c("Virus_Type"),numcolwise(sum)) #includes all columns
vogueA3 <- ddply(vogueA2,c("Virus_Type"),numcolwise(sum)) #includes all columns
vogueB3 <- ddply(vogueB2,c("Virus_Type"),numcolwise(sum)) #includes all columns
vogue1b2b <- ddply(vogue1b2a,c("Virus_Type"),numcolwise(sum)) #includes all columns

#write to file
# write.csv(everyone3, "virus_types_all.csv")
# write.csv(vogueA3, "virus_types_1A.csv")
# write.csv(vogueB3, "virus_types_1B.csv")
# write.csv(vogue1b2b, "virus_types_1B2.csv")
