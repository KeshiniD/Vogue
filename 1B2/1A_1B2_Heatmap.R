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
hc3<-hclust(d2, "ward.D2") #species 

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

#subset in alternative way
#subset d and d2 and then cluster with subset?


#flipped axes based on transposed data set
a <- plot(annHeatmap2(as.matrix(data),col=rgb.palette(16), legend=3, breaks=10, 
                      dendrogram=list(Row=list(dendro=as.dendrogram(hc3)), 
                                      Col=list(dendro=as.dendrogram(hr3)),
                                      status='hidden'), labels=list(Col=list(cex=0.1))))
#cex = font size; and set to just column
#dendrogram
#copied from above
hclust(d, method = "ward.D", members = NULL) #wards or average deemed best
w <- hclust(d, method = "ward.D", members = NULL) #method: dif clustering methods
plot(w, labels = NULL, hang =-1, check = TRUE, #hang changes length of bars
     axes = TRUE, frame.plot = FALSE, ann = TRUE,#ann is labels
     main = "Cluster Dendrogram",
     sub = NULL, xlab = "Participants", ylab = "Distance", cex=0.1) #altered font size
rect.hclust(w, k=6, border="red") #puts red border around samples; can select # 

#adding the remaining participants
#took from cleanup and used some code there
#write this data to file
write.table(dataall, "complete1B2data_extra.csv", sep = ",", row.names = FALSE, quote = FALSE)

#assigned bacterial groups and called back
data <- read.csv(file.path("complete1B2data_extra.csv"))
#grouped based on bacteria labels
cdata <- ddply(data,"Group",numcolwise(sum))

#write this table to file
write.table(cdata, "1B2data_groups_extra.csv", sep = ",", row.names = FALSE, quote = FALSE)

#merge 1A with this 1B2 dataset
#1A microbiome data
data1A <- read.delim(file.path("Vogue1A_microbiomedata.txt"))
data1B2 <- read.csv(file.path("1B2data_groups_extra.csv"))

#gathering like bacterial species                    
data1A <- ddply(data1A,c("Bacterial.Species"),numcolwise(sum)) #all columns

#change headers
data1A <- dplyr::rename(data1A, Total.Frequency = Total.reads)
data1B2 <- dplyr::rename(data1B2, Bacterial.Species = Group)

#this one merges the data frame 
zz<-join(data1B2, data1A, type="full")
zz[is.na(zz)] <- 0 # makes NAs into 0s


#aggregates the like observations from merged data frame
data1A_1B2 <- ddply(zz,c("Bacterial.Species"),numcolwise(sum)) #everything


#rename headers
data1A_1B2 <- dplyr::rename(data1A_1B2, Vogue1B2.01.26 = X1B2_01_26, 
                      Vogue1B2.01.35 = X1B2_01_35, Vogue1B2.01.37 = X1B2_01_37, 
                      Vogue1B2.01.38 = X1B2_01_38, Vogue1B2.01.50 = X1B2_01_50, 
                      Vogue1B2.01.52 = X1B2_01_52, Vogue1B2.01.56 = X1B2_01_56, 
                      Vogue1B2.01.58 = X1B2_01_58, Vogue1B2.01.61 = X1B2_01_61, 
                      Vogue1B2.01.62 = X1B2_01_62, Vogue1B2.01.63 = X1B2_01_63, 
                      Vogue1B2.01.64 = X1B2_01_64)


#order columns numerically
data1A_1B2 <- data1A_1B2[,order(names(data1A_1B2))]

#write this data to file
write.table(data1A_1B2, "data1A_1B2_extra.csv", sep = ",", row.names = FALSE, quote = FALSE)

#now use this dataset in making heatmap and clustering
#call dataset
data <- read.csv(file.path("data1A_1B2_extra.csv"))

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
     sub = NULL, xlab = "Participants", ylab = "Distance", cex=0.1)
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

a <- plot(annHeatmap2(as.matrix(data2),col=rgb.palette(14), legend=3, breaks=10, 
                      dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), 
                                      Col=list(dendro=as.dendrogram(hc3)),
                                      status='hidden'), labels=list(Col=list(nrow=13))))