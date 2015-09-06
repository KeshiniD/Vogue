#load data
total <- read.csv(file.path("1B2metabac_v3.csv"))

#packages
library(vegan)

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
a <- distance(vmb, method = "jsd")
m_matrix <- data.matrix(vmb) #make data into matrix #can rename headers
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
rect.hclust(w, k=6, border="red") #puts red border around samples 

#heatmap with jsd
hr3<- hclust(d, method = "ward.D2", members = NULL) #participants
hc3<-hclust(d2, "complete") #species #just altered above code, didn't rewrite

#colours for groups
rgb.palette <- colorRampPalette(c("black", "blue", "yellow", "red"), space = "rgb")#colour for heatmaps

clus.col<-c("blue4"   ,   "green"  ,  "orange" ,"honeydew3"  ,"red" ,"royalblue1", "yellow")#colours for the groups

plot(annHeatmap2(as.matrix(vmb),col=rgb.palette(12), legend=3, breaks=10, 
                 dendrogram=list(Row=list(dendro=as.dendrogram(hr3)), 
                                 Col=list(dendro=as.dendrogram(hc3)),
                                 status='hidden'), labels=list(Col=list(nrow=13))))
