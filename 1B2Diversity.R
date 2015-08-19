#Shannon Diversity
library(vegan)
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)
library(entropart)
library(epitools)

#call for data
data <- read.csv(file.path("1B2.csv"))

#just have the bacterial species
bac <- data %>%
  select(Lactobacillus.crispatus, Lactobacillus.gasseri, 
         Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, 
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)

H <- diversity(bac) #shannon for individuals
View(H)

#write data to file (altered headings and called back)
write.table(H, "1B2_individual_diversity.csv", sep = ",", row.names = FALSE, quote = FALSE)
div <- read.csv(file.path("1B2_individual_diversity.csv"))

#cohort
#first need to make new table with total counts for each bacterial species
#bac counts
data2 <-
  gather(data, key = 'Bacteria', value = 'Counts', Lactobacillus.crispatus, 
         Lactobacillus.gasseri, Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, 
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)

#then use in diversity
H2 <- data2 %>% 
  select (Bacteria, Counts) %>% #WORKED!!!! for cohort
  group_by(Bacteria) %>%
  summarize(TotalCounts = sum(Counts)) %>%
  select (TotalCounts) #stay in for diversity,can remove to see bac
F2 <- diversity(H2)
View(F2)

#write data to file (altered headings and called back)
write.table(F2, "1B2_cohort_diversity.csv", sep = ",", row.names = FALSE, quote = FALSE)
cdiv <- read.csv(file.path("1B2_cohort_diversity.csv"))

#Pielou's eveness
J<- H/log(specnumber(bac)) # for individuals
View(J)

#write data to file (altered headings and called back)
write.table(J, "1B2_individual_Pielou.csv", sep = ",", row.names = FALSE, quote = FALSE)
Piel <- read.csv(file.path("1B2_individual_Pielou.csv"))

J2 <- F2/log(specnumber(H2, MARGIN = 1)) #not working for entire cohort
#specnumber want number of species and we can manually enter 21 to get J2
#fixed it with below code, and can be used for diversity
View(J2)

# want to sum up all bacteria in one row, with bacteria as columns
# can then use this in specnumber() and then pielou for cohort
h4 <- colSums(H2)
H5 <-  t(h4)
J2 <- F2/log(H5) 
View(J2)

#write data to file (altered headings and called back)
write.table(J2, "1B2_cohort_Pielou.csv", sep = ",", row.names = FALSE, quote = FALSE)
cPiel <- read.csv(file.path("1B2_cohort_Pielou.csv"))


#Good's coverage; need to figure this out
## need entropart package

data(Paracou618) #example
Ns <- Paracou618.MC$Ns
Coverage(Ns)

Ns <- H3$TotalCounts#apply to own data
Coverage(Ns, Estimator = Turing) #Ns has to be numeric vector
Coverage(Ns) # is value correct; value could be for entire cohort

#individuals
Ns <- H2
Coverage(Ns, Estimator = Turing) #Ns has to be numeric vector
Coverage(Ns) #default Zhaung

#the way you aren't suppose to do it
vmb2 <- vmb[c(1:21), ]
vmb3 <- vmb2[2:3]
Ns <- vmb3$Counts
Coverage(Ns, Estimator = "Turing") #works

#may have to do separate manually and calculate each
f <- vmb$Participants
a <- split(vmb, f)#need to get this into a vector (the split frames)
# this subsets data based on participants :)
newdata <- vmb[ which(vmb$Participants=='Vogue1B2.1.29'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
Coverage(Ns, Estimator = "Turing") 
#insert each participant for coverage