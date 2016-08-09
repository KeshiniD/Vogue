#load packages
library(plyr)
library(dplyr)
library(tidyr)
library(vegan)
library(Heatplus)
library(ecodist)
library(phyloseq)
library(stats)

#load data
data <- read.delim(file = "JHdata.txt")

#drop participants don't want
data$Vogue1B2_01_02 <- NULL
data$Vogue1B2_01_03 <- NULL
data$Vogue1B2_01_04 <- NULL
data$Vogue1B2_01_05 <- NULL
data$Vogue1B2_01_13 <- NULL
data$Vogue1B2_01_14 <- NULL
data$Vogue1B2_01_16 <- NULL
data$Vogue1B2_01_20 <- NULL
data$Vogue1B2_01_24 <- NULL

#transposed and bacterial species as row names
rownames(data) <- data[,1]
data[,1] <- NULL
data2 <- as.data.frame(t(data)) #transposed and converted into data.frame

#participants
m_matrix <- data.matrix(data2) #make data into matrix #can rename headers
m_matrix <- t(m_matrix) #transpose for participants
a <- otu_table(m_matrix, taxa_are_rows=TRUE)#needs to be in otu table

c <- (distance(a, method = "jsd")) #Jensen-Shannon
d <- sqrt(c) #as per JH's protocol

#makes a dendrogram
hclust(d, method = "ward.D2", members = NULL) #wards or average deemed best
w <- hclust(d, method = "ward.D2", members = NULL) #method: dif clustering methods
plot(w, labels = NULL, hang =-1, check = TRUE, #hang changes length of bars
     axes = TRUE, frame.plot = FALSE, ann = TRUE,#ann is labels
     main = "Cluster Dendrogram",
     sub = NULL, xlab = "Participants", ylab = "Distance", cex=0.1)
rect.hclust(w, k=6, border="red") #puts red border around samples; can select # 

#JH used ward


#does it look the same if I just do the 1B2 participants I am intersted in?
#no