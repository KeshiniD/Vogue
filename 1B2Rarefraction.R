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