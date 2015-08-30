source("https://bioconductor.org/biocLite.R")
biocLite("Heatplus")
install.packages("BiodiversityR", dependencies = TRUE)

#the following packages will need to be installed before you can load the libraries
library(lattice)
library(vegan)
library(BiodiversityR)
library(RColorBrewer)
library(Heatplus)#from Bioconductor not CRAN

#######################
#load the data
#######################
###File Name: 1081Assembly78Cutoff.csv

78% Cutoff

hiv.data<-read.csv(file.choose(), header=T, row.names=1)#this reads the file and makes the ids the rownames
head(hiv.data)#shows the top of the file

#####################
#calculate distance matrix

hiv.dist<-vegdist(hiv.data)#defaults to bray-curtis dissimilarity

#create hierarchecal clusters

hr1<-hclust(vare.dist, "complete")
hr2<-hclust(vare.dist, "single")
hr3<-hclust(vare.dist, "aver")#use this one
par(mfrow=c(1,3))
plot(hr1, hang=-1)
plot(hr2, hang=-1)
plot(hr3, hang=-1)

c7<-cutree(hr3, 7)#cuts the tree into 3 clusters
h1<-cutree(hr3, h=0.85)
h2<-cutree(hr3, h=0.82)
summary(factor(h2))
summary(factor(h1))
c8<-cutree(hr3, 4)
c8
c7
01-001 01-002 01-003 01-004 01-005 01-006 01-007 01-008 01-009 01-010 
     1      2      3      3      4      4      2      5      1      4 
01-011 01-012 01-013 01-014 01-015 01-016 01-017 01-018 01-019 01-020 
     1      1      5      4      3      6      1      1      1      4 
01-021 01-022 01-023 01-024 01-025 01-026 01-027 01-028 01-029 01-030 
     1      1      5      1      5      1      4      1      1      1 
01-031 01-032 01-033 01-034 01-035 01-036 01-037 01-038 01-039 01-040 
     1      6      1      4      1      1      1      6      4      1 
01-041 01-042 01-043 01-044 01-045 01-046 01-047 01-048 01-049 01-050 
     4      4      3      1      5      1      1      4      2      1 
01-051 01-052 01-053 01-054 
     7      4      6      5 

summary(factor(c7))
 1  2  3  4  5  6  7 
24  3  4 12  6  4  1 

#make clusters for the species (not necessary, but makes heatmap nicer)

hiv.dist2<-vegdist(t(a))#makes distance matrix on transposed data

hc1<-hclust(hiv.dist2, "complete")
hc2<-hclust(hiv.dist2, "single")
hc3<-hclust(hiv.dist2, "average")#use this one
quartz()
par(mfrow=c(1,3))
plot(hc1, hang=-1)
plot(hc2, hang=-1)
plot(hc3, hang=-1)

#make the heatmaps

rgb.palette <- colorRampPalette(c("black", "blue", "yellow", "red"), space = "rgb")#colour for heatmaps

colors()#gives list of R colours
clus.col2<-c( 'blue', 'deepskyblue3', 'cornflowerblue', 'deepskyblue', 'green3', 
             'forestgreen', 'palegreen', 'green', 'darkgoldenrod1', 
             'purple', 'mediumorchid2', 'plum', 'firebrick', 'firebrick1', 
             'gray33', 'gray', 'mediumvioletred', 'black', 'olivedrab2', 
             'orange3', 'tomato', 'lightsalmon', 'slateblue', 'turquoise', 
             'lavender', 'rosybrown2', 'deeppink')#colours for the groups

quartz(height=8.5, width=12)
plot(annHeatmap2(as.matrix(vmb),col=rgb.palette(11), legend=3, breaks=10, dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), Col=list(dendro=as.dendrogram(hc3))), cluster=list(Row=list(cuth=0.85),col=clus.col2), labels=list(Col=list(nrow=20.3))), widths=c(1,7.5), heights=c(1,1,9.5))
#need to figure out colours

#, ann=list(Row=list(data=convAnnData(hiv.clin[,2])))

#load the data
#######################
###File Name: 1081Assembly90Cutoff.csv

90% Cutoff

hiv.dat<-read.csv(file.choose(), header=T, row.names=1)#this reads the file and makes the ids the rownames
head(hiv.dat)#shows the top of the file

#####################
#calculate distance matrix

hiv.distance<-vegdist(hiv.dat)#defaults to bray-curtis dissimilarity

#create hierarchecal clusters

hir1<-hclust(hiv.distance, "complete")
hir2<-hclust(hiv.distance, "single")
hir3<-hclust(hiv.distance, "aver")#use this one
par(mfrow=c(1,3))
plot(hir1, hang=-1)
plot(hir2, hang=-1)
plot(hir3, hang=-1)

cut7<-cutree(hir3, 7)#cuts the tree into 3 clusters
cut7
01-001 01-002 01-003 01-004 01-005 01-006 01-007 01-008 01-009 01-010 
     1      2      1      3      4      4      2      5      1      4 
01-011 01-012 01-013 01-014 01-015 01-016 01-017 01-018 01-019 01-020 
     1      1      5      4      1      6      1      1      1      4 
01-021 01-022 01-023 01-024 01-025 01-026 01-027 01-028 01-029 01-030 
     1      1      5      1      5      1      4      1      1      1 
01-031 01-032 01-033 01-034 01-035 01-036 01-037 01-038 01-039 01-040 
     1      6      1      4      1      1      1      6      4      1 
01-041 01-042 01-043 01-044 01-045 01-046 01-047 01-048 01-049 01-050 
     4      4      1      1      5      7      1      4      2      1 
01-051 01-052 01-053 01-054 
     7      4      6      5 

summary(factor(cut7))
 1  2  3  4  5  6  7 
26  3  1 12  6  4  2 

#make clusters for the species (not necessary, but makes heatmap nicer)

hiv.distance2<-vegdist(t(hiv.dat))#makes distance matrix on transposed data

hic1<-hclust(hiv.distance2, "complete")
hic2<-hclust(hiv.distance2, "single")
hic3<-hclust(hiv.distance2, "average")#use this one
quartz()
par(mfrow=c(1,3))
plot(hic1, hang=-1)
plot(hic2, hang=-1)
plot(hic3, hang=-1)

#make the heatmaps

rgb.palette <- colorRampPalette(c("black", "blue", "yellow", "red"), space = "rgb")#colour for heatmaps

colors()#gives list of R colours
clus.col3<-c( "blue4"   ,   "green"  ,  "orange" ,"honeydew3"  ,"seagreen" ,"royalblue1", "yellow" , "red" )#colours for the groups

quartz(height=8.5, width=12)
plot(annHeatmap2(as.matrix(hiv.dat),col=rgb.palette(12), legend=3, breaks=10, dendrogram=list(Row=list(dendro=as.dendrogram(hir3)), Col=list(dendro=as.dendrogram(hic3))), cluster=list(Row=list(cuth=0.85),col=clus.col3), labels=list(Col=list(nrow=20))), widths=c(1,7.5), heights=c(1,1,7.5))

#############90% Clustering Different Height

hiv.dat<-read.csv(file.choose(), header=T, row.names=1)#this reads the file and makes the ids the rownames
head(hiv.dat)#shows the top of the file

#####################
#calculate distance matrix

hiv.distance<-vegdist(hiv.dat)#defaults to bray-curtis dissimilarity

#create hierarchecal clusters

hir1<-hclust(hiv.distance, "complete")
hir2<-hclust(hiv.distance, "single")
hir3<-hclust(hiv.distance, "aver")#use this one
par(mfrow=c(1,3))
plot(hir1, hang=-1)
plot(hir2, hang=-1)
plot(hir3, hang=-1)

cut.75<-cutree(hir3, h=0.75)

summary(factor(cut.75))

> summary(factor(cut.75))
 1  2  3  4  5  6  7  8  9 10 
14  3  3  1 12  6  9  4  1  1 

#make clusters for the species (not necessary, but makes heatmap nicer)

hiv.distance2<-vegdist(t(hiv.dat))#makes distance matrix on transposed data

hic1<-hclust(hiv.distance2, "complete")
hic2<-hclust(hiv.distance2, "single")
hic3<-hclust(hiv.distance2, "average")#use this one
quartz()
par(mfrow=c(1,3))
plot(hic1, hang=-1)
plot(hic2, hang=-1)
plot(hic3, hang=-1)

#make the heatmaps

rgb.palette <- colorRampPalette(c("black", "blue", "yellow", "red"), space = "rgb")#colour for heatmaps

colors()#gives list of R colours
clus.col3<-c( "blue4"   ,   "green"  ,  "orange" ,"yellow"  ,"seagreen" ,"black", "honeydew3" , "lightpink2" , "royalblue1", "red" )#colours for the groups

quartz(height=8.5, width=12)
plot(annHeatmap2(as.matrix(hiv.dat),col=rgb.palette(12), legend=3, breaks=10, dendrogram=list(Row=list(dendro=as.dendrogram(hir3)), Col=list(dendro=as.dendrogram(hic3))), cluster=list(Row=list(cuth=0.75),col=clus.col3), labels=list(Col=list(nrow=20))), widths=c(1,7.5), heights=c(1,1,7.5))
