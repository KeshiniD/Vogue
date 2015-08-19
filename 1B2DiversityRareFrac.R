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

#rarefraction curves
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

##all the codes work, need to understand it
quantile(rowSums(bac))
rowSums(bac)
Srar <- rarefy(bac, min(rowSums(bac)))
View(Srar)
S2 <- rarefy(bac, 2)
View(S2)
all(rank(Srar) == rank(S2)) #same output as vegan doc
range(diversity(bac, "simp") - (S2 -1))

#plots curve but error in lengths; not as nice as rarecurve()
source("http://www.jennajacobs.org/R/rarefaction.txt")
emend.rare<-rarefaction(bac, color=TRUE, legend = TRUE)

#trying to plot; understand arguments
diversity(bac, index = "shannon", MARGIN = 1, base = exp(1)) #also calc Shannon
rarefy(bac, sample = 2, se = FALSE, MARGIN = 1) #set sample to integer (should be smaller than sample size)
rrarefy(bac, sample = 20) #set to integer
drarefy(bac, sample = 5) #set to integer
#need to figure out colours; done
col <- c("black", "darkred", "forestgreen", "orange", "blue", "yellow", 
         "hotpink", "red", "grey", "purple", "white")
rarecurve(bac, step = 1, sample = 2, xlab = "Sample Size", ylab = "Species",
          label = TRUE, col = col, xlim=c(0,15000)) # will plot, set sample and step to integer
#need to figure out legend (do not think there is one)
# can alter x-axis to see some samples more distinctly 

fisher.alpha(bac, MARGIN = 1)
specnumber(bac, MARGIN = 1) #works for H5
