#rarefraction curves
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

##all the codes work
quantile(rowSums(bac))
rowSums(bac) #for each bacteria
Srar <- rarefy(bac, min(rowSums(bac))) #individuals seen in the 0% quantile
#number is subsample size for rarefying community
View(Srar)

#can see differences in species richness with different sample sizes
S2 <- rarefy(bac, 2)
View(S2)
all(rank(Srar) == rank(S2)) #same output as vegan doc (should be False)

#plots curve but error in lengths; not as nice as rarecurve()
source("http://www.jennajacobs.org/R/rarefaction.txt")
emend.rare<-rarefaction(bac, color=TRUE, legend = TRUE)

#trying to plot; understand argument specifics
diversity(bac, index = "shannon", MARGIN = 1, base = exp(1)) #also calc Shannon
#set sample to integer (should be smaller than sample size)
#should probabaly set sample size to min(rowSums(bac))?
rarefy <- rarefy(bac, sample=min(rowSums(bac))) 

#generates randomly rarefied community of size sample
#can see proportions of species in each paricipant based on rarefraction proportion
#expected species richness in random subsample; relative values
rrarefy <- rrarefy(bac, sample=min(rowSums(bac)))

#probability that species occurs in rarefied community of size sample
drarefy <- drarefy(bac, sample = min(rowSums(bac)))

#write individual species richness into file
rarefy <- as.data.frame(rarefy)
write.csv(rarefy, "1B2richness_individual.csv")

#richness for cohort
#already have bac subset data
#first need to make new table with total counts for each bacterial species
#bac counts
bac2 <-
  gather(bac, key = 'Bacteria', value = 'Counts', Lactobacillus.crispatus, 
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
bac3 <- bac2 %>% 
  select (Bacteria, Counts) %>% 
  group_by(Bacteria) %>%
  summarize(TotalCounts = sum(Counts)) %>%
  select (TotalCounts)
#specnumber
h <- colSums(bac3)
h2 <-  t(h)

#cohort richness
rarefy2 <- rarefy(bac3, sample=min(rowSums(bac3))) #may be h
#write cohort species richness into file
rarefy2 <- as.data.frame(rarefy2)
write.csv(rarefy2, "1B2richness_cohort.csv")

#Plot rarefraction cureve
bac <- data %>%
  select(Participants, Lactobacillus.crispatus, Lactobacillus.gasseri, 
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
#want rownames to be participants
rownames(total) <- total[,1]
#colours for individuals
col <- c("black", "darkred", "forestgreen", "orange", "blue", "yellow", 
         "hotpink", "red", "grey", "purple", "white")
rarecurve(bac, step = 1, sample = min(rowSums(bac)), xlab = "Sample Size", ylab = "Species",
          label = TRUE, col = col, xlim=c(0,15000)) # will plot, set sample and step to integer
#each line is rarefied richness value (higher values more rich than lower values)
#xaxis:# of indvidual species present in each participant
#yaxis: bacterial species and amount each participant has
#need to figure out legend (do not think there is one)
#can alter x-axis to see some samples more distinctly 

fisher.alpha(bac, MARGIN = 1)
specnumber(bac, MARGIN = 1) #number of species, can use above for cohort
