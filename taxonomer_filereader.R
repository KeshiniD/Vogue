#to read taxonomer files for virome, subset only viral species

a <- read.delim(file = "Vogue1B_01_RNA_1.full.txt")
library(plyr)
suppressPackageStartupMessages(library(dplyr)) 
library(tidyr)
library(knitr)

a2  <-  a[which(a$bacterial == 'viral'),]
write.csv(a2, "virometemp1b.csv")
