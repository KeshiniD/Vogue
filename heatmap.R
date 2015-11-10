#call for data
data <- read.csv(file.path("data1A_1B2_extra.csv"))

#load packages
library(plyr)
library(dplyr)
library(tidyr)
library(vegan)
library(Heatplus)
library(ecodist)
library(phyloseq)
library(stats)

#remove Total.Frequency, and transpose dataset
data$Total.Frequency <- NULL

#transposed
rownames(data) <- data[,1]
data[,1] <- NULL
data2 <- as.data.frame(t(data)) #transposed and converted into data.frame

#participants
m_matrix <- data.matrix(data2) #make data into matrix #can rename headers
m_matrix <- t(m_matrix) #transpose for participants
a <- otu_table(m_matrix, taxa_are_rows=TRUE)#needs to be in otu table

#Jensen-Shannon distance matrix 
c <- distance(a, method = "jsd")
d <- sqrt(c)

#makes a dendrogram
hclust(d, method = "ward.D2", members = NULL) #wards or average deemed best
w <- hclust(d, method = "ward.D2", members = NULL) #method: dif clustering methods
plot(w, labels = NULL, hang =-1, check = TRUE, #hang changes length of bars
     axes = TRUE, frame.plot = FALSE, ann = TRUE,#ann is labels
     main = "Cluster Dendrogram",
     sub = NULL, xlab = "Participants", ylab = "Distance", cex=0.1)#cex is font size
rect.hclust(w, k=6, border="red") #puts red border around samples; can select # 


#species
m_matrix2 <- data.matrix(data2) #make data into matrix #can rename headers
a2 <- otu_table(m_matrix2, taxa_are_rows=TRUE)#needs to be in otu table

c2 <- distance(a2, method = "jsd")
d2 <- sqrt(c2)

#works makes a dendrogram
hclust(d2, method = "ward.D2", members = NULL) #wards or average deemed best
w <- hclust(d2, method = "ward.D2", members = NULL) #method: dif clustering methods
plot(w, labels = NULL, hang =-1, check = TRUE, #hang changes length of bars
     axes = TRUE, frame.plot = FALSE, ann = TRUE,#ann is labels
     main = "Cluster Dendrogram",
     sub = NULL, xlab = "Participants", ylab = "Distance")
rect.hclust(w, k=6, border="red") #puts red border around samples; can select #

#heatmap with jsd
hr3<- hclust(d, method = "ward.D2", members = NULL) #participants
hc3<-hclust(d2, "ward.D2") #species 

#colours for groups
rgb.palette <- colorRampPalette(c("black", "blue", "yellow", "red"), space = "rgb")#colour for heatmaps

clus.col<-c("blue4"   ,   "green"  ,  "orange" ,"honeydew3"  ,"red" ,"royalblue1", "yellow")#colours for the groups

plot(annHeatmap2(as.matrix(data2),col=rgb.palette(14), legend=3, breaks=10, 
                      dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), 
                                      Col=list(dendro=as.dendrogram(hc3)),
                                      status='hidden'), labels=list(Col=list(nrow=13))))
#can switch axis using dataset=data