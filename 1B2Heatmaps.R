#call for data
total <- read.csv(file.path("1B2metbac_v2.csv"))

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
          Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
          Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
          Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
          Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
          Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)
a<- sweep(vmb, 1, (rowSums(vmb))/100, '/') # calculates for us
vare.dist <- vegdist(a) #works!
str(vare.dist)
summary(vare.dist)

#different distance matrix; euclidean
d <- dist(as.matrix(vmb2)) #gives dif dendrogram