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

#plots curve but error in lengths; not as nice as rarecurve()
source("http://www.jennajacobs.org/R/rarefaction.txt")
emend.rare<-rarefaction(bac, color=TRUE, legend = TRUE)

#Plot rarefraction curve
viral <- data %>%
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
rownames(viral) <- viral[,1]
viral[,1] <- NULL

#colours for individuals
col <- c('deepskyblue3', 'cornflowerblue', 'deepskyblue', 'green3', 
         'forestgreen', 'palegreen', 'green', 'darkgoldenrod1', 
         'purple', 'mediumorchid2', 'plum', 'firebrick', 'yellow', 'firebrick1', 
         'gray33', 'gray', 'mediumvioletred', 'black', 'olivedrab2', 
         'orange3', 'tomato', 'lightsalmon', 'slateblue', 'turquoise', 
         'lavender', 'rosybrown2', 'deeppink')

#step: 1+sample size
rarecurve(viral, step = 27, sample = min(rowSums(viral)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Viral Species", 
          label = FALSE, col = col, xlim=c(0,17500), lwd = 2)
#each line is rarefied richness value (higher values more rich than lower values)
#xaxis:# of indvidual species present in each participant
#yaxis: bacterial species and amount each participant has
#need to figure out legend (do not think there is one)
#can alter x-axis to see some samples more distinctly 

specnumber(viral, MARGIN = 1) #number of species, can use above for cohort

#section earlier curves to see more indepth
rarecurve(viral, step = 27, sample = min(rowSums(viral)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Viral Species", 
          label = FALSE, col = col, xlim=c(0,8500), lwd = 2)

#to calculate slope of rarecurve (derivatibe of rarefy) at given sample size
#sample size, the row sum for each participant (No this creates slope of zero)
# a <- as.data.frame(rareslope(viral, (rowSums(viral)-1))) #what make sample size equal to?

###################################
#June-11-16
#rareslope to see if curves are platleaus
#slope at sample size = total reads for each participant
slope <- rareslope(viral, rowSums(viral)) #result is zero

#looks at diagonal results which this case is relevant results for each participant
diag(slope) 

#sample size = total reads minus 1
slope1 <- rareslope(viral, (rowSums(viral)-1))
diag(slope1)
#sample size = total reads minus 5
slope5 <- rareslope(viral, (rowSums(viral)-5))
diag(slope5)
#sample size = total reads minus 10
slope10 <- rareslope(viral, (rowSums(viral)-10))
diag(slope10)
#sample size = total reads minus 100
slope100 <- rareslope(viral, (rowSums(viral)-100))
diag(slope100)

#make into data.frame, merge, and write to file
slope1_df <- data.frame(diag(slope1))
slope5_df <- data.frame(diag(slope5))
slope10_df <- data.frame(diag(slope10))
slope100_df <- data.frame(diag(slope100))

z <- join(slope1_df, slope5_df, type = "full")
zz <- join(slope10_df, z, type = "full")
zzz <- join(slope100_df, zz, type = "full")

#write.csv(zzz, "rareslope_virome.csv") #edit made to the excel document

#############
#Aug-5-16
#adding legend
par(mar=c(5,6,7,2)) #and making space for it; alter margins

rarecurve(viral, step = 27, sample = min(rowSums(viral)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Viral Species", 
          label = FALSE, col = col, xlim=c(0,8500), lwd = 2, cex.lab=1.5) 

rarecurve(viral, step = 27, sample = min(rowSums(viral)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Bacterial Species", 
          label = FALSE, col = col, xlim=c(0,17500), lwd = 2, cex.lab=1.5)

legend(x=0,y=30,legend=paste(c("A_52", "A_59", "A_61", "A_62", "A_64", "A_65", 
                               "A_68", "A_69", "A_70", "A_71", "A_74", "A_75", 
                               "A_76", "A_77", "A_78", "A_81", "A_84", "A_85", 
                               "A_92", "A_101", "A_106", "B_01", "B_03", "B_04", 
                               "B_05", "B_06", "B_08", "B_09", "B_11", "B_12", 
                               "B_13", "B_15", "B_17", "B_21", "B_26", "B_27", 
                               "B_32", "B_34", "B_36", "B_37", "B_38", "B_40", 
                               "B_43", "B_48", "B_51", "B_52", "1B2_06", "1B2_07", 
                               "1B2_08", "1B2_09", "1B2_10", "1B2_11", "1B2_12", 
                               "1B2_15")),
       pch=16, col = col,
       bty="n",ncol=13,cex=1,
       pt.cex=2,xpd=TRUE, text.width = 1, y.intersp = 0.2)
#pch:type of symbol, bty?, cex:font size of text, pt.cex:size of points
#xpd:put legend outside of plot, text.width:space between points
#y.intersp:space between rows, ncol:number of columns, x&y:coord of legend position
