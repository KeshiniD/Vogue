phage <- read.csv("Phage_species_DNA.csv")
vDNA <- read.csv("Viral_species_DNA.csv")
vRNA <- read.csv("Viral_species_RNA.csv")

#omit empty column
vDNA$X <- NULL
vRNA$X <- NULL
phage$X <- NULL

#merge
everyone <- join(vDNA, vRNA, type="full")
everyone <- join(phage, everyone, type="full")

#consolidate families together
everyone_2 <- ddply(everyone,c("Var1"),numcolwise(sum)) #includes all columns

#rename Var1
everyone_2 <- dplyr::rename(everyone_2, Viral_Species = Var1)

#remove #1 (root)
everyone_3 <- everyone_2[!everyone_2$Viral_Species == "root",]

#write to file
# write.csv(everyone_3, "DNA_RNA_phage_viral_species_all.csv")
