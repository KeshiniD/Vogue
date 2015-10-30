#1B2 microbiome data
data1B2a <- read.delim(file.path("1b2_01_29_redone.txt"))
data1B2b <- read.delim(file.path("1b2_26_64_redone.txt"))

#1A microbiome data
data1A <- read.delim(file.path("Vogue1A_microbiomedata.txt"))

#load packages
library(plyr)
library(dplyr)
library(tidyr)

#gathering like bacterial species                    
data1A <- ddply(data1A,c("Bacterial.Species"),numcolwise(sum)) #all columns
data1B2a <- ddply(data1B2a,c("Bacterial.Species"),numcolwise(sum)) #all columns
data1B2b <- ddply(data1B2b,c("Bacterial.Species"),numcolwise(sum)) #all columns

#this one merges the data frame 
zz<-join(data1B2a, data1B2b, type="full")
zz2<-join(zz, data1A, type="full")
zz[is.na(zz)] <- 0 # makes NAs into 0s
zz2[is.na(zz2)] <- 0

#aggregates the like observations from merged data frame
data1A_1B2 <- ddply(zz2,c("Bacterial.Species"),numcolwise(sum)) #everything

#write this data to file
write.table(data1A_1B2, "data1A_1B2.csv", sep = ",", row.names = FALSE, quote = FALSE)

#changed a few variables names 
#BVAB2 and Megaspheara so that they would all be the same and aggregate
#call file back and aggregate
data <- read.csv(file.path("data1A_1B2.csv"))
data <- ddply(data,c("Bacterial.Species"),numcolwise(sum)) #everything

#rename headers
data <- dplyr::rename(data, Total.Reads = Total.reads, Vogue1B2.01.26 = X1B2_01_26, 
                      Vogue1B2.01.35 = X1B2_01_35, Vogue1B2.01.37 = X1B2_01_37, 
                      Vogue1B2.01.38 = X1B2_01_38, Vogue1B2.01.50 = X1B2_01_50, 
                      Vogue1B2.01.52 = X1B2_01_52, Vogue1B2.01.56 = X1B2_01_56, 
                      Vogue1B2.01.58 = X1B2_01_58, Vogue1B2.01.61 = X1B2_01_61, 
                      Vogue1B2.01.62 = X1B2_01_62, Vogue1B2.01.63 = X1B2_01_63, 
                      Vogue1B2.01.64 = X1B2_01_64)

#remove excluded participants



#write this to file
write.table(data, "data1A_1B2.csv", sep = ",", row.names = FALSE, quote = FALSE)

