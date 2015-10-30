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

#write this to file
write.table(data, "data1A_1B2.csv", sep = ",", row.names = FALSE, quote = FALSE)

