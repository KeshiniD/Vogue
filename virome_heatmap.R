# Load packages you will need with library() command. 
library(lattice)
library(vegan)
library(BiodiversityR)
library(RColorBrewer)
library(pheatmap)

# This last package is from a different source. For the first install, you need:
# source("http://bioconductor.org/biocLite.R")
# biocLite("Heatplus")
library(Heatplus)

#For indicator species analysis
library(labdsv)

# for heatmap.2
library(gplots) 


#################################### Load data

# Saved excel file as .csv
# OTUs are ROWS and SAMPLES are COLUMNS.
# Frequencies reads must be proporational data (%).

data <- read.csv("DNA_RNA_phage_viral_species_minus_papillomaviridae_all.csv")

data[is.na(data)] <- 0
data$X <- NULL
row.names(data) <- data[, 1]
data <- data[, -1]

#need to be proportional data
data <- prop.table(as.matrix(data), margin= 2) #make into dataframe after

################################ Basic Heatmap (without clustering)

# Turn the table so that OTU are the columns and Samples are the rows 
# (some calculations later on this code will ask for a transposed table, better have it done already)

data_t <-t(data)
head(data_t)

# Set heatmap color 

# OPTION 1:
# scalecolor <- colorRampPalette(c("black","yellow","red"), space = "rgb")(256) 

# OPTION 2:
scalecolor <- colorRampPalette(c("lightyellow", "orange", "red"), space = "rgb")(100)

# The margins command sets the width of the white space around the plot. 
# The first element is the bottom margin and the second is the right margin
# In this case, I have long labels and no need for the same names, so I'll make
# the bottom margin big and the right margin none

heatmap(as.matrix(data),
        Rowv = NA, 
        Colv = NA, 
        cexRow=0.5,
        cexCol=0.4,
        col = scalecolor, 
        margins = c(4, 10)  )



###############################
#  Clustering (Jensen-Shannon)
###############################

# To cluster the samples, we need to calculate the distance metric we want to use 
# (Bray-Curtis or in this case, Jensen-Shannon distance) and then cluster those 
# distances based on a method (in this case, Ward 2 method)

# This is the code to set-up the Jensen-Shannon Distance calculation (because not built into R)
# This code always stays the same - just run this section to load the dist.JSD variable

dist.JSD <- function(inMatrix, pseudocount=0.000001, ...) {
  KLD <- function(x,y) sum(x *log(x/y))
  JSD<- function(x,y) sqrt(0.5 * KLD(x, (x+y)/2) + 0.5 * KLD(y, (x+y)/2))
  matrixColSize <- length(colnames(inMatrix))
  matrixRowSize <- length(rownames(inMatrix))
  colnames <- colnames(inMatrix)
  resultsMatrix <- matrix(0, matrixColSize, matrixColSize)
  
  inMatrix = apply(inMatrix,1:2,function(x) ifelse (x==0,pseudocount,x))
  
  for(i in 1:matrixColSize) {
    for(j in 1:matrixColSize) {
      resultsMatrix[i,j]=JSD(as.vector(inMatrix[,i]),
                             as.vector(inMatrix[,j]))
    }
  }
  colnames -> colnames(resultsMatrix) -> rownames(resultsMatrix)
  as.dist(resultsMatrix)->resultsMatrix
  attr(resultsMatrix, "method") <- "dist"
  return(resultsMatrix)
}


# then to make the distance matrix you input your data
# the calculation is always on the columns, so to get the OTU calculated, use transposed:
OTU.dist <- dist.JSD(data_t)

# and to get the women (samples) calculated, use the original table:
sample.dist <- dist.JSD(data)

# then to cluster
data_Jensen_Shannon_dist_OTU_cluster <- hclust(OTU.dist, method = "ward.D2", members = NULL)
data_Jensen_Shannon_dist_sample_cluster <- hclust(sample.dist, method = "ward.D2")

# Clustering only samples
heatmap(as.matrix(data_t), 
        Rowv = as.dendrogram(data_Jensen_Shannon_dist_sample_cluster),
        cexRow=0.5,
        cexCol=0.4,
        col = scalecolor, 
        margins = c(8, 2)
)

# Clustering OTU and samples
heatmap(as.matrix(data_t), 
        Rowv = as.dendrogram(data_Jensen_Shannon_dist_sample_cluster),
        Colv = as.dendrogram(data_Jensen_Shannon_dist_OTU_cluster), 
        cexRow=0.5,
        cexCol=0.4,
        col = scalecolor, 
        margins = c(9, 2)
)



###########################
# INDICATOR SPECIES - CST
###########################

# Load package and its dependecies.
library(labdsv)

# Do clustering analysis for samples (by Bray-Curtis or Jensen-Shannon) (if you haven't yet).

# To see the clusters, use the plot() function.
# X11 is the graphics viewer for PC. Quartz is the graphics viewer for Mac.
# The hang=-1 to make the tree lengths line up at the bottom.
# quartz()
windows()
plot(data_Jensen_Shannon_dist_sample_cluster, hang=-1)

# Make clusters by cutting the tree at a specific point (Ex. 24). 
# Look at the dendrogram to make that decision. 
# Depending where you cut the tree, you will have different number of clusters.
data_24 <- cutree(data_Jensen_Shannon_dist_sample_cluster, h = 24)
summary(factor(data_24))

data_clust_7 <- cutree(data_Jensen_Shannon_dist_sample_cluster, k=7) #since 7 clusters with 1A&1B2
summary(factor(data_clust_7))

# Save the list of cluster results 
# write.csv(data_clust_7, "data_clust_7.csv")
#did this for k=4 and wrote to file

#importance(t(data), data_24) #cluster_7 in here with JH data, species does in column
#write.csv(importance(t(data), data_24), "data_24_indicator.csv")
#wrote with k=4, and edited row names myself

#######################
# Adding coloured bar 
#######################

# Import the category file (sample names must be in the same order as the input table)



#########################################################
# Nugent category

# Load the Nugent category file
nugent.coded <- read.csv("nugent.coded.csv", header = TRUE)
head(nugent.coded)

# Sample_ID             nugent     Color
# 1 Vogue1C-02-003    Intermediate BV steelblue
# 2 Vogue1C-02-009    Intermediate BV steelblue
# 3 Vogue1C-02-016 Consistent with BV      blue
# 4 Vogue1C-02-030    Intermediate BV steelblue


# Remember that the column heading for the category we want is called 'nugent'
# Create a list variable to put the NUGENT category information in
metadata <- list()

# Put the values of the Nugent categories (from the imported file column header) 
# from our imported table matrix$column (Nugent.coded$nugent)
# and recreate a column in our new list with the a column name (metadata$nugent)
metadata$nugent <- nugent.coded$nugent

# convert the list variable into a factor variable
metadata$nugent <- factor(metadata$nugent)

# Check to see how many different factors you have and what they are
levels(metadata$nugent)

# Should be: [1] "Consistent with BV" "Intermediate BV" "Missing data" "Not consistent with BV"

# Assign a colour to each level in order
levels(metadata$nugent)[1] <- "blue"
levels(metadata$nugent)[2] <- "steelblue"
levels(metadata$nugent)[3] <- "white"
levels(metadata$nugent)[4] <- "lightblue"

# Check that each factor got a colour
levels(metadata$nugent)

# Should be: [1] "blue" "steelblue" "white" "lightblue"

# Convert those colour labels to characters be read by the program
metadata$nugent <- as.character(metadata$nugent)


# Then add it to the heatmap as RowSideColours
# Also adding na.rm - remove missing values
# Also adding trace - adds lines between rows and columns - set to none
# Also adding density.info - for the heatmap legend - adds extra info not needed
# Also adding lhei - legend height - indicates how high the heatmap key will be
# Also adding key.xlab - x-axis label for heatmap legend



#########################################################
# Community State Type

# Now we need to import the CST file
cst.coded <- read.csv("cst.coded.csv", header = TRUE)

head(cst.coded)

cst.coded

# Sample_ID        cst     Color
# 1 Vogue1C.02.003 IVC forestgreen
# 2 Vogue1C.02.009 IVD        blue
# 3 Vogue1C.02.016 IVC forestgreen
# 4 Vogue1C.02.030 IVD        blue


# Remember that the column heading for the category we want is called 'cst'
# Create a list variable to put the NUGENT category information in
metadata <- list()

# Put the values of the CST categories (from the imported file column header) 
# from our imported table matrix$column (cst.coded$cst)
# and recreate a column in our new list with the a column name (metadata$cst)
metadata$cst <- cst.coded$cst

# convert the list variable into a factor variable
metadata$cst <- factor(metadata$cst)

# Check to see how many different factors you have and what they are
levels(metadata$cst)

# Should be: [1] "I" "II" "III" "IVC" "IVD" "V"

# Assign a colour to each level in order
levels(metadata$cst)[1] <- "orange"
levels(metadata$cst)[2] <- "purple"
levels(metadata$cst)[3] <- "magenta"
levels(metadata$cst)[4] <- "forestgreen"
levels(metadata$cst)[5] <- "blue"
levels(metadata$cst)[6] <- "red"

# Check that each factor got a colour
levels(metadata$cst)

# Should be: [1] "orange" "purple" "magenta" "forestgreen" "blue" [6] "red" 

# Convert those colour labels to characters be read by the program
metadata$cst <- as.character(metadata$cst)


######################
# Final Heatmap
######################

# Don't forget to change the last argument "RowSideColors" 
# depending on which variable you want to put in annotation (CST or Nugent)
# If you don't want add a coloured bar with the extra info, 
# remove "RowSideColors" or "ColSideColors" from the heatmap code.


# Clustering only samples
heatmap.2(as.matrix(data_t),
          Rowv = as.dendrogram(data_Jensen_Shannon_dist_sample_cluster),
          dendrogram = "row",
          col = scalecolor,
          na.rm = TRUE,
          trace = "none",
          density.info = "none",
          lhei = c(2, 8),
          lwid = c(2, 4),
          key.title = NA,
          key.xlab = "Relative proportion", 
          margins = c(20, 0.5), 
          RowSideColors = metadata$cst
)


# Clustering samples and OTUs
heatmap.2(as.matrix(data_t),
          Rowv = as.dendrogram(data_Jensen_Shannon_dist_sample_cluster),
          Colv = as.dendrogram(data_Jensen_Shannon_dist_OTU_cluster), 
          col = scalecolor,
          na.rm = TRUE,
          trace = "none",
          density.info = "none",
          lhei = c(2, 8),
          lwid = c(2, 4),
          key.title = NA,
          key.xlab = "Relative proportion", 
          margins = c(20, 0.5),
          RowSideColors = metadata$cst
)

#### Transposing
# Biggest thing to see is that the col.clus and row.clus get put in 
# the opposite row/column labels

# Clustering only samples (and without branch tree for OTUs)
heatmap.2(t(as.matrix(data_t)),
          Colv = as.dendrogram(data_Jensen_Shannon_dist_sample_cluster),
          dendrogram = "col",
          col = scalecolor,
          na.rm = TRUE,
          trace = "none",
          density.info = "none",
          lhei = c(2, 8),
          lwid = c(2, 8),
          key.title = NA,
          key.xlab = "Relative proportion",
          margins = c(0.5, 20), 
          ColSideColors = metadata$cst
)


# Clustering samples and OTUs (included both branches)
heatmap.2(t(as.matrix(data_t)),
          Rowv = as.dendrogram(data_Jensen_Shannon_dist_OTU_cluster),
          Colv = as.dendrogram(data_Jensen_Shannon_dist_sample_cluster),
          col = scalecolor,
          na.rm = TRUE,
          trace = "none",
          density.info = "none",
          lhei = c(2, 9),
          lwid = c(2, 8),
          key.title = NA,
          key.xlab = NA,
          margins = c(0.5, 18),
          ColSideColors = metadata$cst
)

####################################################################################