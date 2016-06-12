#to read taxonomer files for virome, subset only viral species

a <- read.delim(file = "Vogue1A_101_RNA_1.full.taxonomer")
library(plyr)
suppressPackageStartupMessages(library(dplyr)) 
library(tidyr)
library(knitr)

a2  <-  a[which(a$bacterial == 'viral'),]
write.csv(a2, "Vogue1A_101_RNA_viromedata.csv")

###

a <- read.delim(file = "Vogue1B_01_RNA_1.full (1).taxonomer")
colnames(a)
a2  <-  a[which(a$bacterial == 'viral'),]
write.csv(a2, "Vogue1B_01_RNA_viromedata.csv")
