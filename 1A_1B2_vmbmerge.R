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

