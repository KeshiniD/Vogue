#install packages
source("https://bioconductor.org/biocLite.R")
biocLite("Heatplus")
install.packages("BiodiversityR", dependencies = TRUE)

#the following packages will need to be installed before you can load the libraries
library(lattice)
library(vegan)
library(BiodiversityR)
library(RColorBrewer)
library(Heatplus)#from Bioconductor not CRAN

#load data
total <- read.csv(file.path("1B2metbac_v2.csv"))

#want rownames to be participants
rownames(total) <- total[,1]


#want bacterial species in decimals;need to make it look like example
#subset in decimals; relative abundance
vmb <- tbl_df(total) %>% #subset
  select (Lactobacillus.crispatus, Lactobacillus.gasseri, Lactobacillus.iners, 
          Lactobacillus.jensenii, Gardnerella.vaginalis.Group.A, 
          Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.C, 
          Gardnerella.vaginalis.Group.D, Actinobacteria.sp., Atopobium.vaginae, 
          Clostridia.sp..BVAB2, Clostridium.genomosp..BVAB3, Escherichia.coli, 
          Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
          Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
          Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
          Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
          Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)
a<- sweep(vmb, 1, (rowSums(vmb))/100, '/') # calculates for us

vare.dist <- vegdist(a) #works!#defaults to bray-curtis dissimilarity

#create hierarchecal clusters
#Participants
hr1<-hclust(vare.dist, "complete")
hr2<-hclust(vare.dist, "single")
hr3<-hclust(vare.dist, "aver")#use this one
par(mfrow=c(1,3)) #to see each plot side by side
plot(hr1, hang=-1)
plot(hr2, hang=-1)
plot(hr3, hang=-1, xlab="Participants") #use this one

#Species, makes heatmap nicer
a2<-vegdist(t(a))#makes distance matrix on transposed data
hc1<-hclust(a2, "complete")
hc2<-hclust(a2, "single")
hc3<-hclust(a2, "average")#use this one
par(mfrow=c(1,1)) #just want to see last plot
plot(hc1, hang=-1)
plot(hc2, hang=-1)
plot(hc3, hang=-1, xlab = "Bacterial Species") # use this one

#Heatmap
plot(annHeatmap2(as.matrix(vmb),col=rgb.palette(11), legend=3, breaks=10, dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), Col=list(dendro=as.dendrogram(hc3))), cluster=list(Row=list(cuth=0.85),col=clus.col2), labels=list(Col=list(nrow=20.3))), widths=c(1,7.5), heights=c(1,1,9.5))
plot(annHeatmap2(as.matrix(vmb), legend=TRUE, breaks=10, 
                 dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), 
                                 Col=list(dendro=as.dendrogram(hc3))), 
                 cluster=list(Row=list(cuth=0.85)), 
                 labels=list(Col=list(nrow=20.3))), widths=c(1,7.5),
     heights=c(1,1,9.5))
plot(annHeatmap2(as.matrix(vmb), 
                 breaks=256, col = g2r.colors))

#can redo heatmaps for different species cutoff #next steps