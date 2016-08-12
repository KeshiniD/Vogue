#redoing with new post-processed files: Aug-10-16
phageD <- read.csv("Phage_family_DNA.csv")
phageR <- read.csv("Phage_family_RNA.csv")
vDNA <- read.csv("Viral_family_DNA.csv")
vRNA <- read.csv("Viral_family_RNA.csv")

#omit empty column
vDNA$X <- NULL
vRNA$X <- NULL
phageD$X <- NULL
phageR$X <- NULL

#merge
everyone <- join(vDNA, vRNA, type="full")
everyone <- join(phageD, everyone, type="full")
everyone <- join(phageR, everyone, type="full")

#consolidate families together
everyone_2 <- ddply(everyone,c("Var1"),numcolwise(sum)) #includes all columns

#rename Var1
everyone_2 <- dplyr::rename(everyone_2, Viral_Family = Var1)

#remove #1 (root)
everyone_3 <- everyone_2[!everyone_2$Viral_Family == "root",]

#write to file
# write.csv(everyone_3, "DNA_RNA_phage_viral_family_all.csv")
