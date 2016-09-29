#load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)

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
vref <- read.csv("recode_ncbicode_family.csv", header=FALSE, stringsAsFactors = FALSE)

#omit empty rows and columns
# vref <- vref %>%
#   select(V1, V2)
vref <- vref[c(2:154),]

#recode ncbi codes to families
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

#gathering like types                    
everyone3 <- ddply(everyone2,c("Viral_Family"),numcolwise(sum)) #includes all columns
vogueA3 <- ddply(vogueA2,c("Viral_Family"),numcolwise(sum)) #includes all columns
vogueB3 <- ddply(vogueB2,c("Viral_Family"),numcolwise(sum)) #includes all columns
vogue1b2b <- ddply(vogue1b2a,c("Viral_Family"),numcolwise(sum)) #includes all columns

#remove '1'
everyone4 <- everyone3[c(2:59),]
vogueA4 <- vogueA3[c(2:45),]
vogueB4 <- vogueB3[c(2:52),]
vogue1b2c <- vogue1b2b[c(2:36),]

#write to file
# write.csv(everyone4, "DNA_RNA_phage_viral_family_all_v2.csv")
# write.csv(vogueA4, "DNA_RNA_phage_viral_family_1A_v2.csv")
# write.csv(vogueB4, "DNA_RNA_phage_viral_family_1B_v2.csv")
# write.csv(vogue1b2c, "DNA_RNA_phage_viral_family_1B2_v2.csv")
