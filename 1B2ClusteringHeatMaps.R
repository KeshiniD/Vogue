#install packages
source("http://bioconductor.org/biocLite.R")
biocLite("Heatplus")
install.packages("BiodiversityR", dependencies = TRUE)
install.packages("Heatplus", dependencies = TRUE)
install.packages("RColorBrewer", dependencies = TRUE)

#the following packages will need to be installed before you can load the libraries
library(lattice)
library(vegan)
library(BiodiversityR)
library(RColorBrewer)
library(Heatplus)#from Bioconductor not CRAN

#load data
total <- read.csv(file.path("1B2metabac_v3.csv"))

#want rownames to be participants
rownames(total) <- total[,1]
total[,1] <- NULL

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

#setting row.names
row.names(vmb) <- c( 'Vogue1B2.01.01', 'Vogue1B2.01.06', 'Vogue1B2.01.07', 
                     'Vogue1B2.01.08', 'Vogue1B2.01.09', 'Vogue1B2.01.10', 
                     'Vogue1B2.01.11', 'Vogue1B2.01.12', 'Vogue1B2.01.15', 
                     'Vogue1B2.01.19', 'Vogue1B2.01.21', 'Vogue1B2.01.23', 
                     'Vogue1B2.01.26', 'Vogue1B2.01.28', 'Vogue1B2.01.29', 
                     'Vogue1B2.01.35', 'Vogue1B2.01.37', 'Vogue1B2.01.38', 
                     'Vogue1B2.01.50', 'Vogue1B2.01.52', 'Vogue1B2.01.56', 
                     'Vogue1B2.01.58', 'Vogue1B2.01.61', 'Vogue1B2.01.62', 
                     'Vogue1B2.01.63', 'Vogue1B2.01.64')

a<- sweep(vmb, 1, (rowSums(vmb))/100, '/') # calculates for us

vare.dist <- vegdist(a) #works!#defaults to bray-curtis dissimilarity
str(vare.dist)
summary(vare.dist)

#create hierarchecal clusters
#Participants
hr1<-hclust(vare.dist, "complete")
hr2<-hclust(vare.dist, "single")
hr3<-hclust(vare.dist, "aver")#use this one
par(mfrow=c(1,1)) #to see each plot side by side
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

#also makes same dendogram
library(stats)
#works makes a dendrogram
hclust(vare.dist, method = "average", members = NULL) #wards or average deemed best
w <- hclust(vare.dist, method = "average", members = NULL) #method: dif clustering methods
plot(w, labels = NULL, hang =-1, check = TRUE, #hang changes length of bars
     axes = TRUE, frame.plot = FALSE, ann = TRUE,#ann is labels
     main = "Cluster Dendrogram",
     sub = NULL, xlab = "Participants", ylab = "Height")
rect.hclust(w, k=6, border="red") #puts red border around samples 
#detects 8 clusters

#validates red boxes
mydata <- scale(vmb)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 1:11) wss[i] <- sum(kmeans(mydata, #doesnt work
                                     centers=i)$withinss)
#works suggests that a 8 cluster method is appropropriate for this group
#value before drop off in slope indicates this value approporaite clustering model
#K values
plot(wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


#Heatmap
#colours for groups
rgb.palette <- colorRampPalette(c("black", "blue", "yellow", "red"), space = "rgb")#colour for heatmaps

clus.col<-c("blue4"   ,   "green"  ,  "orange" ,"honeydew3"  ,"red" ,"royalblue1", "yellow")#colours for the groups

#works but no custom colours
plot(annHeatmap2(as.matrix(vmb), legend=3, breaks=10, dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), Col=list(dendro=as.dendrogram(hc3))), cluster=list(Row=list(cuth=0.85)), labels=list(Col=list(nrow=20.3))), widths=c(1,7.5), heights=c(1,1,9.5))

#trying to figure out labels and legends
plot(annHeatmap2(as.matrix(vmb),col=rgb.palette(12), legend=3, breaks=10, 
                 dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), 
                                 Col=list(dendro=as.dendrogram(hc3))), 
                 cluster=list(Row=list(cuth=0.85),col=clus.col), 
                 labels=list(Col=list(nrow=20.3))))

#another heatmap #figure out labels and dendogram
vmb <- tbl_df(total) %>% #subset same as above
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
m_matrix <- data.matrix(vmb) #make data into matrix #can rename headers
m_matrix <- t(m_matrix) #transpose
a <- otu_table(m_matrix, taxa_are_rows=TRUE)#needs to be in otu table
plot_heatmap(a)

#can redo heatmaps for different species cutoff #next steps

#heatpmap without dendrograms and include participant names
plot(annHeatmap2(as.matrix(vmb),col=rgb.palette(12), legend=3, breaks=10, 
                 dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), 
                                 Col=list(dendro=as.dendrogram(hc3)),
                                 status='hidden'), labels=list(Col=list(nrow=13))))
#ordered in participant order (not clustered)
plot(annHeatmap2(as.matrix(vmb),col=rgb.palette(12), legend=3, breaks=10, 
                 dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), 
                                 Col=list(dendro=as.dendrogram(hc3)),
                                 status='no'), labels=list(Col=list(nrow=13))))
#ordered via CST
vmb[,"Participant"]  <- c('CST.II.Vogue1B2.01.01', 'CST.IVA.Vogue1B2.01.06', 
                    'CST.III.Vogue1B2.01.07', 'CST.III.Vogue1B2.01.08', 'CST.III.Vogue1B2.01.09', 
                    'CST.IVD.Vogue1B2.01.10', 'CST.IVC.Vogue1B2.01.11', 'CST.I.Vogue1B2.01.12', 
                    'CST.I.Vogue1B2.01.15', 'CST.IVA.Vogue1B2.01.19', 'CST.III.Vogue1B2.01.21', 
                    'CST.III.Vogue1B2.01.23', 'CST.IVC.Vogue1B2.01.26', 'CST.I.Vogue1B2.01.28', 
                    'CST.IVC.Vogue1B2.01.29', 'CST.IVC.Vogue1B2.01.35', 'CST.IVC.Vogue1B2.01.37', 
                    'CST.IVA.Vogue1B2.01.38', 'CST.III.Vogue1B2.01.50', 'CST.IVA.Vogue1B2.01.52', 
                    'CST.IVA.Vogue1B2.01.56', 'CST.I.Vogue1B2.01.58', 'CST.IVD.Vogue1B2.01.61', 
                    'CST.IVC.Vogue1B2.01.62', 'CST.I.Vogue1B2.01.63', 'CST.IVA.Vogue1B2.01.64')
vmb2 <- vmb %>%
  arrange(Participant)
row.names(vmb2) <- c('CST.I.Vogue1B2.01.12',   'CST.I.Vogue1B2.01.15','CST.I.Vogue1B2.01.28', 
               'CST.I.Vogue1B2.01.58', 'CST.I.Vogue1B2.01.63', 'CST.II.Vogue1B2.01.01', 
               'CST.III.Vogue1B2.01.07', 'CST.III.Vogue1B2.01.08', 'CST.III.Vogue1B2.01.09', 
               'CST.III.Vogue1B2.01.21', 'CST.III.Vogue1B2.01.23', 'CST.III.Vogue1B2.01.50', 
               'CST.IVA.Vogue1B2.01.06', 'CST.IVA.Vogue1B2.01.19', 'CST.IVA.Vogue1B2.01.38', 
               'CST.IVA.Vogue1B2.01.52', 'CST.IVA.Vogue1B2.01.56', 'CST.IVA.Vogue1B2.01.64', 
               'CST.IVC.Vogue1B2.01.11', 'CST.IVC.Vogue1B2.01.26', 'CST.IVC.Vogue1B2.01.29', 
               'CST.IVC.Vogue1B2.01.35', 'CST.IVC.Vogue1B2.01.37', 'CST.IVC.Vogue1B2.01.62', 
               'CST.IVD.Vogue1B2.01.10', 'CST.IVD.Vogue1B2.01.61')
vmb2$Participant <- NULL
plot(annHeatmap2(as.matrix(vmb2),col=rgb.palette(12), legend=3, breaks=10, 
                 dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), 
                                 Col=list(dendro=as.dendrogram(hc3)),
                                 status='no'), labels=list(Col=list(nrow=13))))

#ordered via Nugent
vmb[,"Participant"]  <- c('N.2.Vogue1B2.01.01', 'N.4.Vogue1B2.01.06', 
                          'N.4.Vogue1B2.01.07', 'N.6.Vogue1B2.01.08', 
                          'N.0.Vogue1B2.01.09', 'N.7.Vogue1B2.01.10', 
                          'N.5.Vogue1B2.01.11', 'N.4.Vogue1B2.01.12', 
                          'N.0.Vogue1B2.01.15', 'N.1.Vogue1B2.01.19', 
                          'N.6.Vogue1B2.01.21', 'N.4.Vogue1B2.01.23', 
                          'N.8.Vogue1B2.01.26', 'N.4.Vogue1B2.01.28', 
                          'N.4.Vogue1B2.01.29', 'N.6.Vogue1B2.01.35', 
                          'N.0.Vogue1B2.01.37', 'N.8.Vogue1B2.01.38', 
                          'N.4.Vogue1B2.01.50', 'N.6.Vogue1B2.01.52', 
                          'N.8.Vogue1B2.01.56', 'N.4.Vogue1B2.01.58', 
                          'N.8.Vogue1B2.01.61', 'N.7.Vogue1B2.01.62', 
                          'N.0.Vogue1B2.01.63', 'N.7.Vogue1B2.01.64')
vmb2 <- vmb %>%
  arrange(Participant)
row.names(vmb2) <- c( 'N.0.Vogue1B2.01.09','N.0.Vogue1B2.01.15', 'N.0.Vogue1B2.01.37',
                      'N.0.Vogue1B2.01.63', 'N.1.Vogue1B2.01.19', 'N.2.Vogue1B2.01.01', 
                      'N.4.Vogue1B2.01.06', 'N.4.Vogue1B2.01.07', 'N.4.Vogue1B2.01.12', 
                      'N.4.Vogue1B2.01.23', 'N.4.Vogue1B2.01.28', 'N.4.Vogue1B2.01.29', 
                      'N.4.Vogue1B2.01.50', 'N.4.Vogue1B2.01.58', 'N.5.Vogue1B2.01.11', 
                      'N.6.Vogue1B2.01.08', 'N.6.Vogue1B2.01.21', 'N.6.Vogue1B2.01.35',
                      'N.6.Vogue1B2.01.52', 'N.7.Vogue1B2.01.10', 'N.7.Vogue1B2.01.62', 
                      'N.7.Vogue1B2.01.64', 'N.8.Vogue1B2.01.26', 'N.8.Vogue1B2.01.38', 
                      'N.8.Vogue1B2.01.56', 'N.8.Vogue1B2.01.61')
vmb2$Participant <- NULL
plot(annHeatmap2(as.matrix(vmb2),col=rgb.palette(12), legend=3, breaks=10, 
                 dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), 
                                 Col=list(dendro=as.dendrogram(hc3)),
                                 status='no'), labels=list(Col=list(nrow=13))))
