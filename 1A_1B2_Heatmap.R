#call dataset
data <- read.csv(file.path("data1A_1B2.csv"))

#load packages
library(plyr)
library(dplyr)
library(tidyr)
library(vegan)
library(Heatplus)
library(ecodist)
library(phyloseq)

#remove Total.Reads, and transpose dataset
data$Total.Reads <- NULL

#transposed
rownames(data) <- data[,1]
data[,1] <- NULL
data2 <- as.data.frame(t(data)) #transposed and converted into data.frame

#participants
m_matrix <- data.matrix(data2) #make data into matrix #can rename headers
m_matrix <- t(m_matrix) #transpose for participants
a <- otu_table(m_matrix, taxa_are_rows=TRUE)#needs to be in otu table

c <- distance(a, method = "jsd")
d <- sqrt(c)

#also makes same dendogram
library(stats)
#works makes a dendrogram
hclust(d, method = "ward.D2", members = NULL) #wards or average deemed best
w <- hclust(d, method = "ward.D2", members = NULL) #method: dif clustering methods
plot(w, labels = NULL, hang =-1, check = TRUE, #hang changes length of bars
     axes = TRUE, frame.plot = FALSE, ann = TRUE,#ann is labels
     main = "Cluster Dendrogram",
     sub = NULL, xlab = "Participants", ylab = "Distance")
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
hc3<-hclust(d2, "ward.D2") #species #just altered above code, didn't rewrite

#colours for groups
rgb.palette <- colorRampPalette(c("black", "blue", "yellow", "red"), space = "rgb")#colour for heatmaps

clus.col<-c("blue4"   ,   "green"  ,  "orange" ,"honeydew3"  ,"red" ,"royalblue1", "yellow")#colours for the groups

a <- plot(annHeatmap2(as.matrix(data2),col=rgb.palette(14), legend=3, breaks=10, 
                 dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), 
                                 Col=list(dendro=as.dendrogram(hc3)),
                                 status='hidden'), labels=list(Col=list(nrow=13))))

#trying to alter layout 
#did not work
dnd = list(Row=list(status="no"), Col=list(status="no"))
ann = list(Row=list(data=a), Col=list(data=1))
ll = heatmapLayout(dendrogram=dnd, annotation=ann, leg.side=NULL, show=TRUE)
ll


#trying to alter margins in pdf format
#did not work
pdf("file.pdf",width = 8, height = 11,paper='special') 

plot(annHeatmap2(as.matrix(data2),col=rgb.palette(14), legend=3, breaks=10, 
                 dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), 
                                 Col=list(dendro=as.dendrogram(hc3)),
                                 status='hidden'), labels=list(Col=list(nrow=13))))
dev.off()

#subset of 1B2 but keep same clustering
data3 <- data2[311:336,]
plot(annHeatmap2(as.matrix(data3),col=rgb.palette(14), legend=3, 
                 breaks=10, dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), 
                Col=list(dendro=as.dendrogram(hc3)),status='hidden'), labels=list(Col=list(nrow=13))))
