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
         Clostridium.genomosp..BVAB3, Escherichia.coli, Eukaryote, 
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
#write.csv(rarefy, "1B2richness_individual.csv")

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
         Clostridium.genomosp..BVAB3, Escherichia.coli, Eukaryote,
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


#cohort richness
rarefy2 <- rarefy(bac3, sample=min(rowSums(bac3))) #may be h
#write cohort species richness into file
rarefy2 <- as.data.frame(rarefy2)
#write.csv(rarefy2, "1B2richness_cohort.csv")b #will fix headings in excel

#Plot rarefraction curve
bac <- data %>%
  select(Participants, Lactobacillus.crispatus, Lactobacillus.gasseri, 
         Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, Eukaryote, 
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)
#want rownames to be participants
rownames(bac) <- bac[,1]
bac[,1] <- NULL
#colours for individuals
col <- c('deepskyblue3', 'cornflowerblue', 'deepskyblue', 'green3', 
             'forestgreen', 'palegreen', 'green', 'darkgoldenrod1', 
             'purple', 'mediumorchid2', 'plum', 'firebrick', 'yellow', 'firebrick1', 
             'gray33', 'gray', 'mediumvioletred', 'black', 'olivedrab2', 
             'orange3', 'tomato', 'lightsalmon', 'slateblue', 'turquoise', 
             'lavender', 'rosybrown2', 'deeppink')
rarecurve(bac, step = 27, sample = min(rowSums(bac)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Bacterial Species", 
          label = FALSE, col = col, xlim=c(0,17500), lwd = 2)
#each line is rarefied richness value (higher values more rich than lower values)
#xaxis:# of indvidual species present in each participant
#yaxis: bacterial species and amount each participant has
#need to figure out legend (do not think there is one)
#can alter x-axis to see some samples more distinctly 

fisher.alpha(bac, MARGIN = 1) #alpha parameter; do not need
specnumber(bac, MARGIN = 1) #number of species, can use above for cohort

#section earlier curves to see more indepth
rarecurve(bac, step = 27, sample = min(rowSums(bac)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Bacterial Species", 
          label = FALSE, col = col, xlim=c(0,8500), lwd = 2)

#to calculate slope of rarecurve (derivatibe of rarefy) at given sample size
#sample size, the row sum for each participant (No this creates slope of zero)
a <- as.data.frame(rareslope(bac, (rowSums(bac)-1))) #what make sample size equal to?

#try something different; can group by participants now but still need to figure
#out sample size, and make rareslope work below
a <- bac2 %>%
  group_by(Participants) %>%
  mutate(tc = sum(Counts))%>%
  summarise(a =   rareslope(bac2, tc))
  

#participants, bacteria and counts
bac2 <-
  gather(data, key = 'Bacteria', value = 'Counts', Lactobacillus.crispatus, 
         Lactobacillus.gasseri, Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, Eukaryote,
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)

#####################################
#Feb-26-16
#trying to calculate dissimilarity of rarefaction curve
beta <- vegdist(bac, binary=TRUE) 
#bac being data.frame of bacteria species headers
#participants rows and filled with counts
mean(beta)
#0.2785247

###################################
#June-11-16
#rareslope to see if curves are platleaus
#slope at sample size = total reads for each participant
slope <- rareslope(bac, rowSums(bac)) #result is zero

#looks at diagonal results which this case is relevant results for each participant
diag(slope) 

#sample size = total reads minus 1
slope1 <- rareslope(bac, (rowSums(bac)-1))
diag(slope1)
#sample size = total reads minus 5
slope5 <- rareslope(bac, (rowSums(bac)-5))
diag(slope5)
#sample size = total reads minus 10
slope10 <- rareslope(bac, (rowSums(bac)-10))
diag(slope10)
#sample size = total reads minus 100
slope100 <- rareslope(bac, (rowSums(bac)-100))
diag(slope100)

#make into data.frame, merge, and write to file
slope1_df <- data.frame(diag(slope1))
slope5_df <- data.frame(diag(slope5))
slope10_df <- data.frame(diag(slope10))
slope100_df <- data.frame(diag(slope100))

z <- join(slope1_df, slope5_df, type = "full")
zz <- join(slope10_df, z, type = "full")
zzz <- join(slope100_df, zz, type = "full")

#write.csv(zzz, "rareslope_1B2.csv") #edit made to the excel document

#############
#Aug-5-16
#adding legend
par(mar=c(5,6,7,2)) #and making space for it; alter margins

rarecurve(bac, step = 27, sample = min(rowSums(bac)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Bacterial Species", 
          label = FALSE, col = col, xlim=c(0,8500), lwd = 2, cex.lab=1.5) 

rarecurve(bac, step = 27, sample = min(rowSums(bac)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Bacterial Species", 
          label = FALSE, col = col, xlim=c(0,17500), lwd = 2, cex.lab=1.5)

legend(x=0,y=30,legend=paste(c("1", "6", "7", "8", "9", "10", "11", "12", "15", 
                               "19", "21", "23", "26", "28", "29", "35", "37", 
                               "38", "50", "52", "56", "58", "61", "62", "63", "64")),
       pch=16, col = col,
       bty="n",ncol=13,cex=1,
       pt.cex=2,xpd=TRUE, text.width = 1, y.intersp = 0.2)
#pch:type of symbol, bty?, cex:font size of text, pt.cex:size of points
#xpd:put legend outside of plot, text.width:space between points
#y.intersp:space between rows, ncol:number of columns, x&y:coord of legend position
