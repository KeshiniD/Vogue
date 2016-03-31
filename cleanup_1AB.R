#to remove collection data and just get HIV data
data <- read.delim(file.path("VOGUE_1B_HIV_data.txt"))
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)
library(assertthat)

vmb <- tbl_df(data) %>% # collection vs HIV
  arrange(Event.Name)

myData = vmb[-c(1:94), ] # just HIV data
# write to file
write.table(myData, "Vogue1B_HIVdata.csv", sep = ",", row.names = FALSE, quote = FALSE)

#clean up 1A file
data <- read.delim(file.path("VOGUE_1A.txt"))
# gather ethnicity
a <- as.factor(data$ethwhite___1, c("1"="Yes", "0"="No"))

data2 <-
  gather(data, key = 'Bacteria', value = 'Counts', Lactobacillus.crispatus, Lactobacillus.iners, Lactobacillus.gasseri, 
         Lactobacillus.jensenii, Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.A, 
         Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.D, 
         Megasphaera.sp.genomosp.type.1, Escherichia.coli, Prevotella.timonensis, Clostridia.sp.BVAB2, 
         Clostridium.genomosp.BVAB3, Atopobium.vaginae, Anaerobes, Other.Clostridia, 
         Other.Bacteroidetes, Other.Proteobacteria, Other.Actinobacteria, Other.Firmicutes, Other)

# Collection 1B data
data <- read.csv(file.path("VOGUE_1B.csv"))
vmb <- tbl_df(data) %>% # collection vs HIV vs blank
  arrange(redcap_event_name)
myData = vmb[c(348:401), ] # just collection
#write to file
write.table(myData, "Vogue1B_collection2.csv", sep = ",", row.names = FALSE, 
            quote = FALSE)
