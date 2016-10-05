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

################
#rarefaction curves for each cohort
#call for data
vogueA <- read.csv("DNA_RNA_phage_viral_species_1A_v3.csv")
vogueB <- read.csv("DNA_RNA_phage_viral_species_1B_v3.csv")
vogue1B2 <- read.csv("DNA_RNA_phage_viral_species_1B2_v3.csv")

#remove extra column
vogueA$X <- NULL
vogueB$X <- NULL
vogue1B2$X <- NULL

#Na to zero
vogueA[is.na(vogueA)] <- 0
vogueB[is.na(vogueB)] <- 0
vogue1B2[is.na(vogue1B2)] <- 0

########################################
#1A
#Plot rarefraction curve

#want rownames to be participants
rownames(vogueA) <- vogueA[,1]
vogueA[,1] <- NULL
vogueA <- as.data.frame(t(vogueA))

#colours for individuals
col <- c('deepskyblue3', 'cornflowerblue', 'deepskyblue', 'green3', 
         'forestgreen', 'palegreen', 'green', 'darkgoldenrod1', 
         'purple', 'mediumorchid2', 'plum', 'firebrick', 'yellow', 'firebrick1', 
         'gray33', 'gray', 'mediumvioletred', 'black', 'olivedrab2', 
         'orange3', 'tomato', 'lightsalmon', 'slateblue', 'turquoise', 
         'lavender', 'rosybrown2', 'deeppink')

# #step: 1+sample size
# rarecurve(vogueA, step = 27, sample = min(rowSums(vogueA)), 
#           xlab = "Sequence Read Counts", ylab = "Number of Different Viral Species", 
#           label = FALSE, col = col, xlim=c(0,1100000), lwd = 2)
# 
# #section earlier curves to see more indepth
# rarecurve(vogueA, step = 27, sample = min(rowSums(vogueA)), 
#           xlab = "Sequence Read Counts", ylab = "Number of Different Viral Species", 
#           label = FALSE, col = col, xlim=c(0,11000), lwd = 2)

####################################
#adding legend
par(mar=c(5,6,7,2)) #and making space for it; alter margins

rarecurve(vogueA, step = 27, sample = min(rowSums(vogueA)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Bacterial Species", 
          label = FALSE, col = col, xlim=c(0,11000), lwd = 2, cex.lab=1.5) 

rarecurve(vogueA, step = 27, sample = min(rowSums(vogueA)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Bacterial Species", 
          label = FALSE, col = col, xlim=c(0,1100000), lwd = 2, cex.lab=1.5)

legend(x=0,y=160,legend=paste(c("52", "59", "61", "62", "64", "65", "68", "69", "70", 
                               "71", "74", "75", "76", "77", "78", "81", 
                               "84", "85", "92", "101", "106")),
       pch=16, col = col,
       bty="n",ncol=11,cex=1,
       pt.cex=2,xpd=TRUE, text.width = 1, y.intersp = 0.2)
#pch:type of symbol, bty?, cex:font size of text, pt.cex:size of points
#xpd:put legend outside of plot, text.width:space between points
#y.intersp:space between rows, ncol:number of columns, x&y:coord of legend position

###############
#1B
rownames(vogueB) <- vogueB[,1]
vogueB[,1] <- NULL
vogueB <- as.data.frame(t(vogueB))

#colours for individuals
col <- c('deepskyblue3', 'cornflowerblue', 'deepskyblue', 'green3', 
         'forestgreen', 'palegreen', 'green', 'darkgoldenrod1', 
         'purple', 'mediumorchid2', 'plum', 'firebrick', 'yellow', 'firebrick1', 
         'gray33', 'gray', 'mediumvioletred', 'black', 'olivedrab2', 
         'orange3', 'tomato', 'lightsalmon', 'slateblue', 'turquoise', 
         'lavender', 'rosybrown2', 'deeppink')

#adding legend
par(mar=c(5,6,7,2)) #and making space for it; alter margins

rarecurve(vogueB, step = 27, sample = min(rowSums(vogueB)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Bacterial Species", 
          label = FALSE, col = col, xlim=c(0,25000), lwd = 2, cex.lab=1.5) 

rarecurve(vogueB, step = 27, sample = min(rowSums(vogueB)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Bacterial Species", 
          label = FALSE, col = col, xlim=c(0,1100000), lwd = 2, cex.lab=1.5)

legend(x=0,y=170,legend=paste(c("1", "3", "4", "5", "6", "8", "9", "11", "12", 
                               "13", "15", "17", "21", "26", "27", "32", 
                               "34", "36", "37", "38", "40", "43", "48", "51", "52")),
       pch=16, col = col,
       bty="n",ncol=13,cex=1,
       pt.cex=2,xpd=TRUE, text.width = 1, y.intersp = 0.2)

###################
#1B2
#want rownames to be participants
rownames(vogue1B2) <- vogue1B2[,1]
vogue1B2[,1] <- NULL
vogue1B2 <- as.data.frame(t(vogue1B2))

#colours for individuals
col <- c('deepskyblue3', 'green3', 'darkgoldenrod1', 'purple', 'firebrick', 'yellow', 
         'gray', 'black')

####################################
#adding legend
par(mar=c(5,6,7,2)) #and making space for it; alter margins

rarecurve(vogue1B2, step = 27, sample = min(rowSums(vogue1B2)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Bacterial Species", 
          label = FALSE, col = col, xlim=c(0,3000), lwd = 2, cex.lab=1.5) 

rarecurve(vogue1B2, step = 27, sample = min(rowSums(vogue1B2)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Bacterial Species", 
          label = FALSE, col = col, xlim=c(0,105000), lwd = 2, cex.lab=1.5)

legend(x=0,y=28,legend=paste(c("1", "6", "7", "8", "9", "10", "11", "12", "15")),
       pch=16, col = col,
       bty="n",ncol=11,cex=1,
       pt.cex=2,xpd=TRUE, text.width = 1, y.intersp = 0.2)

###############################################################################################
###############################################################################################

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

###################################################################################
#call for data
vogueA <- read.csv("DNA_RNA_phage_viral_family_1A_v2.csv")
vogueB <- read.csv("DNA_RNA_phage_viral_family_1B_v2.csv")
vogue1B2 <- read.csv("DNA_RNA_phage_viral_family_1B2_v2.csv")

#remove extra column
vogueA$X <- NULL
vogueB$X <- NULL
vogue1B2$X <- NULL

#Na to zero
vogueA[is.na(vogueA)] <- 0
vogueB[is.na(vogueB)] <- 0
vogue1B2[is.na(vogue1B2)] <- 0

#want rownames to be participants
rownames(vogueA) <- vogueA[,1]
vogueA[,1] <- NULL
vogueA <- as.data.frame(t(vogueA))

rownames(vogueB) <- vogueB[,1]
vogueB[,1] <- NULL
vogueB <- as.data.frame(t(vogueB))

rownames(vogue1B2) <- vogue1B2[,1]
vogue1B2[,1] <- NULL
vogue1B2 <- as.data.frame(t(vogue1B2))

#################################################
#1A: rarefaction slopes
slope <- rareslope(vogueA, rowSums(vogueA)) #result is zero
#looks at diagonal results which this case is relevant results for each participant
diag(slope) 

#sample size = total reads minus 1
slope1 <- rareslope(vogueA, (rowSums(vogueA)-1))
diag(slope1)
#sample size = total reads minus 5
slope5 <- rareslope(vogueA, (rowSums(vogueA)-5))
diag(slope5)
#sample size = total reads minus 10
slope10 <- rareslope(vogueA, (rowSums(vogueA)-10))
diag(slope10)
#sample size = total reads minus 100
slope100 <- rareslope(vogueA, (rowSums(vogueA)-100))
diag(slope100)

#make into data.frame, merge, and write to file
slope1_df <- data.frame(diag(slope1))
slope5_df <- data.frame(diag(slope5))
slope10_df <- data.frame(diag(slope10))
slope100_df <- data.frame(diag(slope100))

z <- join(slope1_df, slope5_df, type = "full")
zz <- join(slope10_df, z, type = "full")
zzz <- join(slope100_df, zz, type = "full")

# write.csv(zzz, "rareslope_virome_1A.csv") #edit made to the excel document
##################################################################################
#1B: rarefaction slopes
slope <- rareslope(vogueB, rowSums(vogueB)) #result is zero
#looks at diagonal results which this case is relevant results for each participant
diag(slope) 

#sample size = total reads minus 1
slope1 <- rareslope(vogueB, (rowSums(vogueB)-1))
diag(slope1)
#sample size = total reads minus 5
slope5 <- rareslope(vogueB, (rowSums(vogueB)-5))
diag(slope5)
#sample size = total reads minus 10
slope10 <- rareslope(vogueB, (rowSums(vogueB)-10))
diag(slope10)
#sample size = total reads minus 100
slope100 <- rareslope(vogueB, (rowSums(vogueB)-100))
diag(slope100)

#make into data.frame, merge, and write to file
slope1_df <- data.frame(diag(slope1))
slope5_df <- data.frame(diag(slope5))
slope10_df <- data.frame(diag(slope10))
slope100_df <- data.frame(diag(slope100))

z <- join(slope1_df, slope5_df, type = "full")
zz <- join(slope10_df, z, type = "full")
zzz <- join(slope100_df, zz, type = "full")

# write.csv(zzz, "rareslope_virome_1B.csv") #edit made to the excel document
#####################################################################################
#1B2: rarefaction slopes
slope <- rareslope(vogue1B2, rowSums(vogue1B2)) #result is zero
#looks at diagonal results which this case is relevant results for each participant
diag(slope) 

#sample size = total reads minus 1
slope1 <- rareslope(vogue1B2, (rowSums(vogue1B2)-1))
diag(slope1)
#sample size = total reads minus 5
slope5 <- rareslope(vogue1B2, (rowSums(vogue1B2)-5))
diag(slope5)
#sample size = total reads minus 10
slope10 <- rareslope(vogue1B2, (rowSums(vogue1B2)-10))
diag(slope10)
#sample size = total reads minus 100
slope100 <- rareslope(vogue1B2, (rowSums(vogue1B2)-100))
diag(slope100)

#make into data.frame, merge, and write to file
slope1_df <- data.frame(diag(slope1))
slope5_df <- data.frame(diag(slope5))
slope10_df <- data.frame(diag(slope10))
slope100_df <- data.frame(diag(slope100))

z <- join(slope1_df, slope5_df, type = "full")
zz <- join(slope10_df, z, type = "full")
zzz <- join(slope100_df, zz, type = "full")

# write.csv(zzz, "rareslope_virome_1B2.csv") #edit made to the excel document
