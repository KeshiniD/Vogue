#redoing with new post-processed files: Aug-10-16
#redone with filtered files: sept 27-16
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

################################################################################
#repeat for 1A, 1B, 1B2 separately
#1A
phageD <- read.csv("phage_family_DNA_1A.csv")
phageR <- read.csv("phage_family_RNA.csv")
vDNA <- read.csv("Viral_family_DNA_1A.csv")
vRNA <- read.csv("Viral_family_RNA_1A.csv")

#omit empty column
vDNA$X <- NULL
vRNA$X <- NULL
phageD$X <- NULL
phageR$X <- NULL
phageR$Vogue1B2.01.11 <- NULL

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
# write.csv(everyone_3, "DNA_RNA_phage_viral_family_1A.csv")

####################
#1B
phageD <- read.csv("phage__family_DNA_1B.csv")
vDNA <- read.csv("Viral__family_DNA_1B.csv")
vRNA <- read.csv("Viral__family_RNA_1B.csv")

#omit empty column
vDNA$X <- NULL
vRNA$X <- NULL
phageD$X <- NULL

#merge
everyone <- join(vDNA, vRNA, type="full")
everyone <- join(phageD, everyone, type="full")

#consolidate families together
everyone_2 <- ddply(everyone,c("Var1"),numcolwise(sum)) #includes all columns

#rename Var1
everyone_2 <- dplyr::rename(everyone_2, Viral_Family = Var1)

#remove #1 (root)
everyone_3 <- everyone_2[!everyone_2$Viral_Family == "root",]

#write to file
# write.csv(everyone_3, "DNA_RNA_phage_viral_family_1B.csv")

#####################
#1B2
phageD <- read.csv("phage_family_DNA_1B2.csv")
phageR <- read.csv("phage_family_RNA.csv")
vDNA <- read.csv("Viral_family_DNA_1B2.csv")
vRNA <- read.csv("Viral_family_RNA_1B2.csv")

#omit empty column
vDNA$X <- NULL
vRNA$X <- NULL
phageD$X <- NULL
phageR$X <- NULL
phageR$Vogue1A.01.61 <- NULL
phageR$Vogue1A.01.101 <- NULL

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
# write.csv(everyone_3, "DNA_RNA_phage_viral_family_1B2.csv")
