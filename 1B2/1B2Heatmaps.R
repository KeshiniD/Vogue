#call for data
total <- read.csv(file.path("1B2metabac_v3.csv"))

#load packages
library(vegan)
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)
library(assertthat)
library(entropart)
library(epitools)
library(ggtree)

#Bray-Curtis
install.packages("ecodist", dependencies = TRUE)
library(ecodist)
# or use vegan package
vegdist(x, method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
        na.rm = FALSE, ...) 
#example
data(varespec)
vare.dist <- vegdist(varespec)

#want bacterial species in decimals;need to make it look like example
#subset in decimals; relative abundance
vmb <- tbl_df(total) %>% #subset
  select (Lactobacillus.crispatus, Lactobacillus.gasseri, Lactobacillus.iners, 
          Lactobacillus.jensenii, Gardnerella.vaginalis.Group.A, 
          Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.C, 
          Gardnerella.vaginalis.Group.D, Actinobacteria.sp., Atopobium.vaginae, 
          Clostridia.sp..BVAB2, Clostridium.genomosp..BVAB3, Escherichia.coli, 
          Eukaryote, Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
          Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
          Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
          Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
          Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)
a<- sweep(vmb, 1, (rowSums(vmb))/100, '/') # calculates for us
vare.dist <- vegdist(a) #works!
str(vare.dist)
summary(vare.dist)

#different distance matrix; euclidean
d <- dist(as.matrix(vmb)) #gives dif dendrogram

#clustering
library(stats)
#works makes a dendrogram
hclust(vare.dist, method = "average", members = NULL) #wards or average deemed best
w <- hclust(vare.dist, method = "average", members = NULL) #method: dif clustering methods
plot(w, labels = NULL, hang =-1, check = TRUE, #hang changes length of bars
     axes = TRUE, frame.plot = FALSE, ann = TRUE,#ann is labels
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height")
rect.hclust(w, k=8, border="red") #puts red border around samples

#dif clustering example
mydata <- scale(vmb)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 1:11) wss[i] <- sum(kmeans(mydata, #doesnt work
                                     centers=i)$withinss)
plot(wss, type="b", xlab="Number of Clusters",#works but ?? suggests that a 8 cluster method is appropropriate for this group
     ylab="Within groups sum of squares")

#validating cluster solutions
install.packages("fpc", dependencies = TRUE)
library(fpc)
cluster.stats(mydata, fit1$cluster, fit2$cluster) #??

#ggtree example
nwk <- system.file("extdata", "sample.nwk", package="ggtree")
x <- readLines(nwk)
cat(substring(x, 1, 56), "\n", substring(x, 57), "\n")
tree <- read.tree(nwk)
ggplot(tree, aes(x, y)) + geom_tree() + theme_tree() + xlab("") + ylab("")
ggtree(tree, color="steelblue", size=0.5, linetype="dotted")

library("gridExtra") #for added features
#how does data need to be formatted for ggtree??!!

#phyloseq
source("https://bioconductor.org/biocLite.R")
biocLite("phyloseq")
library(phyloseq)
theme_set(theme_bw()) #what does this do?
#also could use ggphylo

#example
data("GlobalPatterns")
#these call for 300 most abundant bacteria taxa
gpt <- subset_taxa(GlobalPatterns, Kingdom=="Bacteria") 
gpt <- prune_taxa(names(sort(taxa_sums(gpt),TRUE)[1:300]), gpt)
plot_heatmap(gpt, sample.label="SampleType")

vmb <- tbl_df(total) %>% #subset same as above
  select (Lactobacillus.crispatus, Lactobacillus.gasseri, Lactobacillus.iners, 
          Lactobacillus.jensenii, Gardnerella.vaginalis.Group.A, 
          Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.C, 
          Gardnerella.vaginalis.Group.D, Actinobacteria.sp., Atopobium.vaginae, 
          Clostridia.sp..BVAB2, Clostridium.genomosp..BVAB3, Escherichia.coli, 
          Eukaryote, Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
          Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
          Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
          Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
          Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)
plot_heatmap(vmb)

#heat maps works!! verison 1
install.packages("Matrix", dependencies = TRUE)
m_matrix <- data.matrix(vmb)
library(Matrix)
heatmap(m_matrix, Colv=NA, scale="column")

#dif dendrogram heatmap version 2
#not for me
cor_t <- cor(t(m_matrix))
distancet <- as.dist(cor_t)
hclust_complete <- hclust(distancet, method = "average")
dendcomplete <- as.dendrogram(hclust_complete)
heatmap(m_matrix, Rowv=dendcomplete, Colv=NA, scale="column")

#version 3
distancem <- dist(m_matrix)
# determining how dendrogram should be ordered
hclust_completem <- hclust(distancem, method = "average") #ward.D, ward.D2 or average
dendcompletem <- as.dendrogram(hclust_completem)
heatmap(m_matrix, Rowv=dendcompletem, Colv=NA, scale="column")
#figure out what is the difference between these three versions
#version 1 and 3 are the same essentially; visually 

#different heat maps; enhanced
#may allow for more control over aspects
install.packages("gplots", dependencies = TRUE)
library(gplots)
heatmap.2(m_matrix, Rowv=dendcompletem, Colv=TRUE, distfun=dist2, hclustfun=hclust2, 
          scale="column", trace="none", dendrogram="row")

#want to calculate dendrogram differently
hclust2 <- function(m_matrix, method="complete", ...)
  hclust(m_matrix, method=method, ...)
dist2 <- function(m_matrix, ...)
  as.dist(1-cor(t(m_matrix), method="pearson")) #pearson, kendall or spearman
heatmap.2(m_matrix, Rowv=dendcompletem, Colv=TRUE, distfun=dist2, 
          hclustfun=hclust2, scale="column", trace="none", 
          dendrogram="both", density.info = "none")
#realize that light means lots, and darker means none
#density.info = histogram/density plot
#key = legend
dist2 <- function(m_matrix, ...) #bray function for heatmap
  vegdist(sweep(m_matrix, 1, (rowSums(m_matrix))/100, '/'))


#make_otu_table and then this is a phylo object
#and can be used in phyloseq, ggphylo and ggtree
install.packages("qiimer", dependencies = TRUE)
library(qiimer)
make_otu_table(m_matrix, sample_ids = NULL)
m_matrix <- t(m_matrix)

#
data(GlobalPatterns)
a <- otu_table(GlobalPatterns)
head(a)
a <- otu_table(m_matrix, taxa_are_rows=TRUE) #works; need transpose
head(a)
str(a)
plot_richness(a)# I am soooo smart :)
plot_tree(a) #needs workd
plot_heatmap(a) #so prettY!!!!
plot_bar(a)
physeq = prune_taxa(taxa_names(a)[1:50], a) #make tree with phy_tree
phy_tree(physeq)
tree <- read.tree(a) #ggtree
ggplot(tree, aes(x, y)) + geom_tree() + theme_tree() + xlab("") + ylab("")


#all works fine
total$Trich..2.months. <- mapvalues(total$Trich..2.months., from = c("0"), to = c("no"))# works
#different example with yes and no
data$abnormal.discharge..y.1..n.0. <- mapvalues(data$abnormal.discharge..y.1..n.0., from = c("0", "1"), to = c("no", "yes"))# works
#with NA
data$oral.sex.in.past.48.hours..y.1..n.0. <- mapvalues(data$oral.sex.in.past.48.hours..y.1..n.0., from = c("0", "1"), to = c("no", "yes"))# works

############################################################################
#June-12-16
#following Aline's code for heatmap
#use JH's data
data <- read.delim(file = "JHdata.txt")
row.names(data) <- data[, 1]
data <- data[, -1]

#follow Aline's code and then
data_clust_7 <- cutree(data_Jensen_Shannon_dist_sample_cluster, k=7) #since 7 clusters with 1A&1B2
summary(factor(data_clust_7))

# Save the list of cluster results 
# write.csv(data_clust_7, "data_clust_7.csv")

#importance(t(data), data_24) #cluster_7 in here with JH data, species does in column
#write.csv(importance(t(data), data_24), "data_24_indicator.csv")

####################
#try with JH data, just looking at 1B2 women #doesnt work
##################
plot(data_Jensen_Shannon_dist_sample_cluster, hang=-5, cex=0.6) #testimage.png
#has right clusters; need to figure how to do just included 1b2

####
#for coloured bars, amend aldex_metadata
#need to add the excluded participants
meta <- read.csv(file="Aldex_metadata_1A_1B2.csv")

#rename dataJH participants names so they look lke aldex metadata participants
data <- dplyr::rename(data, Vogue1B2.01.26 = X1B2_01_26,
                      Vogue1B2.01.35 = X1B2_01_35, Vogue1B2.01.37 = X1B2_01_37,
                      Vogue1B2.01.38 = X1B2_01_38, Vogue1B2.01.50 = X1B2_01_50,
                      Vogue1B2.01.52 = X1B2_01_52, Vogue1B2.01.56 = X1B2_01_56,
                      Vogue1B2.01.58 = X1B2_01_58, Vogue1B2.01.61 = X1B2_01_61,
                      Vogue1B2.01.62 = X1B2_01_62, Vogue1B2.01.63 = X1B2_01_63,
                      Vogue1B2.01.64 = X1B2_01_64, Vogue1B2.01.01 = Vogue1B2_01_01, 
                      Vogue1B2.01.02 = Vogue1B2_01_02, Vogue1B2.01.03 =  Vogue1B2_01_03, 
                      Vogue1B2.01.04 = Vogue1B2_01_04, Vogue1B2.01.05 = Vogue1B2_01_05, 
                      Vogue1B2.01.06 = Vogue1B2_01_06, Vogue1B2.01.07 = Vogue1B2_01_07, 
                      Vogue1B2.01.08 = Vogue1B2_01_08, Vogue1B2.01.09 = Vogue1B2_01_09, 
                      Vogue1B2.01.10 = Vogue1B2_01_10, Vogue1B2.01.11 = Vogue1B2_01_11, 
                      Vogue1B2.01.12 = Vogue1B2_01_12, Vogue1B2.01.13 = Vogue1B2_01_13, 
                      Vogue1B2.01.14 = Vogue1B2_01_14, Vogue1B2.01.15 = Vogue1B2_01_15, 
                      Vogue1B2.01.16 = Vogue1B2_01_16, Vogue1B2.01.19 = Vogue1B2_01_19, 
                      Vogue1B2.01.20 = Vogue1B2_01_20, Vogue1B2.01.21 = Vogue1B2_01_21, 
                      Vogue1B2.01.23 = Vogue1B2_01_23, Vogue1B2.01.24 = Vogue1B2_01_24, 
                      Vogue1B2.01.28 = Vogue1B2_01_28, Vogue1B2.01.29 = Vogue1B2_01_29)

#making new data.frame for excluded participants and then merge
heat_map <- meta %>% select (Participants, Nugent.score, CST)
Participants <-  c("Vogue1B2.01.02","Vogue1B2.01.03", "Vogue1B2.01.04", 
                   "Vogue1B2.01.05", "Vogue1B2.01.13", "Vogue1B2.01.14", 
                   "Vogue1B2.01.16", "Vogue1B2.01.20", "Vogue1B2.01.24")
Nugent.score <- c(3, 6, 0, 0, 7, 2, 1, 0, 7)
CST <- c("II", "IVD", "I", "III", "V", "I", "II", "I", "IVD")
heat <- data.frame(Participants, Nugent.score, CST)

zz <- join(heat, heat_map, type="full")

#make new data.frame for Nugent.score and CST separately
Nugent <- zz %>% 
  select (Participants, Nugent.score)

CST <- zz %>%
  select(Participants, CST)

#group nugent scores, and write categories to file
Nugent$Nugent.score.cat[Nugent$Nugent.score < 4] <- "Inconsistent"
Nugent$Nugent.score.cat[Nugent$Nugent.score >= 4 & Nugent$Nugent.score <=6] <- "Intermediate" 
Nugent$Nugent.score.cat[Nugent$Nugent.score >= 7] <- "Consistent"

Nugent <- Nugent %>%
  select(Participants, Nugent.score.cat)

#write.csv(Nugent, "Nugent_heatmap.csv")
#write CST to file
#write.csv(CST, "CST_heatmap.csv")

#these variables files work with heatmap; need 7th levelfor CST