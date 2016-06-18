recoderFunc <- function(data, oldvalue, newvalue) {
  
  # convert any factors to characters
  
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  
  # create the return vector
  
  newvec <- data
  
  # put recoded values into the correct position in the return vector
  
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  
  newvec
  
}

recoderFunc(data, oldvalue, newvalue)
#set stringsAsFactors=FALSE when readin files, if function does not work

#read viral reference manual
vref <- read.csv(file="virus_ref.csv", stringsAsFactors=FALSE, header=FALSE)
#omit empty rows and columns
vref <- vref %>%
  select(V1, V2)
vref <- vref[c(1:105),]

#load above function
#############################################################################################

#data for everyone together
#load viral DNA, RNA and Phage family data
vDNA <- read.csv(file="Viral_family_DNA.csv", stringsAsFactors=FALSE)
vRNA <- read.csv(file="Viral_family_RNA.csv", stringsAsFactors = FALSE)
phage <- read.csv(file="phage_family_DNA.csv", stringsAsFactors = FALSE)

#omit empty column
vDNA$X <- NULL
vRNA$X <- NULL
phage$X <- NULL

#recode
vDNA2 <- recoderFunc(vDNA, vref$V1, vref$V2)
vRNA2 <- recoderFunc(vRNA, vref$V1, vref$V2)
phage2 <- recoderFunc(phage, vref$V1, vref$V2)

#write to file
# write.csv(vDNA2, "Viral_family_DNA_postref.csv")
# write.csv(vRNA2, "Viral_family_RNA_postref.csv")
# write.csv(phage2, "phage_family_DNA_postref.csv")
#calculate freq of counts and call back

#load data
vDNA <- read.csv(file="Viral_family_DNA_postref.csv", header=TRUE)
vRNA <- read.csv(file="Viral_family_RNA_postref.csv", header=TRUE)
phage <- read.csv(file="phage_family_DNA_postref.csv", header=TRUE)

#omit empty column
vDNA$X <- NULL
vRNA$X <- NULL
phage$X <- NULL

#gathering like species                    
vDNA2 <- ddply(vDNA,c("Var1"),numcolwise(sum)) #includes all columns
vRNA2 <- ddply(vRNA,c("Var1"),numcolwise(sum)) #includes all columns
phage2 <- ddply(phage,c("Var1"),numcolwise(sum)) #includes all columns

#write these three to file
# write.csv(vDNA2, "Viral_family_DNA_consolidated.csv")
# write.csv(vRNA2, "Viral_family_RNA_consolidated.csv")
# write.csv(phage2, "phage_family_DNA_consolidated.csv")

############################################################################################
#repeat above for 1A, 1B, and 1B2

###########
#1A
#load viral DNA, RNA and Phage 1A family data
vDNA_A <- read.csv(file="Viral_family_DNA_1A.csv", stringsAsFactors=FALSE)
vRNA_A <- read.csv(file="Viral_family_RNA_1A.csv", stringsAsFactors = FALSE)
phage_A <- read.csv(file="phage_family_DNA_1A.csv", stringsAsFactors = FALSE)

#omit empty column
vDNA_A$X <- NULL
vRNA_A$X <- NULL
phage_A$X <- NULL

#recode
vDNA_A2 <- recoderFunc(vDNA_A, vref$V1, vref$V2)
vRNA_A2 <- recoderFunc(vRNA_A, vref$V1, vref$V2)
phage_A2 <- recoderFunc(phage_A, vref$V1, vref$V2)

#write to file
# write.csv(vDNA_A2, "Viral_family_DNA_1A_postref.csv")
# write.csv(vRNA_A2, "Viral_family_RNA_1A_postref.csv")
# write.csv(phage_A2, "phage_family_DNA_1A_postref.csv")
#calculate freq of counts and call back

#load data
vDNA_A <- read.csv(file="Viral_family_DNA_1A_postref.csv", header=TRUE)
vRNA_A <- read.csv(file="Viral_family_RNA_1A_postref.csv", header=TRUE)
phage_A <- read.csv(file="phage_family_DNA_1A_postref.csv", header=TRUE)

#omit empty column
vDNA_A$X <- NULL
vRNA_A$X <- NULL
phage_A$X <- NULL

#gathering like species                    
vDNA_A2 <- ddply(vDNA_A,c("Var1"),numcolwise(sum)) #includes all columns
vRNA_A2 <- ddply(vRNA_A,c("Var1"),numcolwise(sum)) #includes all columns
phage_A2 <- ddply(phage_A,c("Var1"),numcolwise(sum)) #includes all columns

#write these three to file
# write.csv(vDNA_A2, "Viral_family_DNA_1A_consolidated.csv")
# write.csv(vRNA_A2, "Viral_family_RNA_1A_consolidated.csv")
# write.csv(phage_A2, "phage_family_DNA_1A_consolidated.csv")

###########
#1B
#load viral DNA, RNA and Phage 1B family data
vDNA_B <- read.csv(file="Viral__family_DNA_1B.csv", stringsAsFactors=FALSE)
vRNA_B <- read.csv(file="Viral__family_RNA_1B.csv", stringsAsFactors = FALSE)
phage_B <- read.csv(file="phage__family_DNA_1B.csv", stringsAsFactors = FALSE)

#omit empty column
vDNA_B$X <- NULL
vRNA_B$X <- NULL
phage_B$X <- NULL

#recode
vDNA_B2 <- recoderFunc(vDNA_B, vref$V1, vref$V2)
vRNA_B2 <- recoderFunc(vRNA_B, vref$V1, vref$V2)
phage_B2 <- recoderFunc(phage_B, vref$V1, vref$V2)

#write to file
# write.csv(vDNA_B2, "Viral_family_DNA_1B_postref.csv")
# write.csv(vRNA_B2, "Viral_family_RNA_1B_postref.csv")
# write.csv(phage_B2, "phage_family_DNA_1B_postref.csv")
#calculate freq of counts and call back

#load data
vDNA_B <- read.csv(file="Viral_family_DNA_1B_postref.csv", header=TRUE)
vRNA_B <- read.csv(file="Viral_family_RNA_1B_postref.csv", header=TRUE)
phage_B <- read.csv(file="phage_family_DNA_1B_postref.csv", header=TRUE)

#omit empty column
vDNA_B$X <- NULL
vRNA_B$X <- NULL
phage_B$X <- NULL

#gathering like species                    
vDNA_B2 <- ddply(vDNA_B,c("Var1"),numcolwise(sum)) #includes all columns
vRNA_B2 <- ddply(vRNA_B,c("Var1"),numcolwise(sum)) #includes all columns
phage_B2 <- ddply(phage_B,c("Var1"),numcolwise(sum)) #includes all columns

#write these three to file
# write.csv(vDNA_B2, "Viral_family_DNA_1B_consolidated.csv")
# write.csv(vRNA_B2, "Viral_family_RNA_1B_consolidated.csv")
# write.csv(phage_B2, "phage_family_DNA_1B_consolidated.csv")

###########
#1B2
#load viral DNA, RNA and Phage 1B family data
vDNA_1B2 <- read.csv(file="Viral_family_DNA_1B2.csv", stringsAsFactors=FALSE)
vRNA_1B2 <- read.csv(file="Viral_family_RNA_1B2.csv", stringsAsFactors = FALSE)
phage_1B2 <- read.csv(file="phage_family_DNA_1B2.csv", stringsAsFactors = FALSE)

#omit empty column
vDNA_1B2$X <- NULL
vRNA_1B2$X <- NULL
phage_1B2$X <- NULL

#recode
vDNA_1B2a <- recoderFunc(vDNA_1B2, vref$V1, vref$V2)
vRNA_1B2a <- recoderFunc(vRNA_1B2, vref$V1, vref$V2)
phage_1B2a <- recoderFunc(phage_1B2, vref$V1, vref$V2)

#write to file
# write.csv(vDNA_1B2a, "Viral_family_DNA_1B2_postref.csv")
# write.csv(vRNA_1B2a, "Viral_family_RNA_1B2_postref.csv")
# write.csv(phage_1B2a, "phage_family_DNA_1B2_postref.csv")
#calculate freq of counts and call back

#load data
vDNA_1B2 <- read.csv(file="Viral_family_DNA_1B2_postref.csv", header=TRUE)
vRNA_1B2 <- read.csv(file="Viral_family_RNA_1B2_postref.csv", header=TRUE)
phage_1B2 <- read.csv(file="phage_family_DNA_1B2_postref.csv", header=TRUE)

#omit empty column
vDNA_1B2$X <- NULL
vRNA_1B2$X <- NULL
phage_1B2$X <- NULL

#gathering like species                    
vDNA_1B2a <- ddply(vDNA_1B2,c("Var1"),numcolwise(sum)) #includes all columns
vRNA_1B2a <- ddply(vRNA_1B2,c("Var1"),numcolwise(sum)) #includes all columns
phage_1B2a <- ddply(phage_1B2,c("Var1"),numcolwise(sum)) #includes all columns

#write these three to file
# write.csv(vDNA_1B2a, "Viral_family_DNA_1B2_consolidated.csv")
# write.csv(vRNA_1B2a, "Viral_family_RNA_1B2_consolidated.csv")
# write.csv(phage_1B2a, "phage_family_DNA_1B2_consolidated.csv")

##########################################################################################
#merge consolidated files for DNA, phage and RNA

#everyone load data
vDNA <- read.csv("Viral_family_DNA_consolidated.csv")
vRNA <- read.csv("Viral_family_RNA_consolidated.csv")
phage <- read.csv("phage_family_DNA_consolidated.csv")

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
everyone_2 <- dplyr::rename(everyone_2, Viral_Families = Var1)

#remove #1 (root)
everyone_3 <- everyone_2[!everyone_2$Viral_Families == "1",]

#write to file
# write.csv(everyone_3, "DNA_RNA_phage_viral_families_all.csv")

###########################################################################################
#1A load data
vDNA_A <- read.csv("Viral_family_DNA_1A_consolidated.csv")
vRNA_A <- read.csv("Viral_family_RNA_1A_consolidated.csv")
phage_A <- read.csv("phage_family_DNA_1A_consolidated.csv")

#omit empty column
vDNA_A$X <- NULL
vRNA_A$X <- NULL
phage_A$X <- NULL

#merge
vogueA <- join(vDNA_A, vRNA_A, type="full")
vogueA <- join(phage_A, vogueA, type="full")

#consolidate families together
vogueA2 <- ddply(vogueA,c("Var1"),numcolwise(sum)) #includes all columns

#rename Var1
vogueA2 <- dplyr::rename(vogueA2, Viral_Families = Var1)

#remove #1 (root)
vogueA3 <- vogueA2[!vogueA2$Viral_Families == "1",]

#write to file
# write.csv(vogueA3, "DNA_RNA_phage_viral_families_1A.csv")

##########################################################################################
#1B load data
vDNA_B <- read.csv("Viral_family_DNA_1B_consolidated.csv")
vRNA_B <- read.csv("Viral_family_RNA_1B_consolidated.csv")
phage_B <- read.csv("phage_family_DNA_1B_consolidated.csv")

#omit empty column
vDNA_B$X <- NULL
vRNA_B$X <- NULL
phage_B$X <- NULL

#merge
vogueB <- join(vDNA_B, vRNA_B, type="full")
vogueB <- join(phage_B, vogueB, type="full")

#consolidate families together
vogueB2 <- ddply(vogueB,c("Var1"),numcolwise(sum)) #includes all columns

#rename Var1
vogueB2 <- dplyr::rename(vogueB2, Viral_Families = Var1)

#remove #1 (root)
vogueB3 <- vogueB2[!vogueB2$Viral_Families == "1",]

#write to file
# write.csv(vogueB3, "DNA_RNA_phage_viral_families_1B.csv")

##########################################################################################
#1B2 load data
vDNA_1B2 <- read.csv("Viral_family_DNA_1B2_consolidated.csv")
vRNA_1B2 <- read.csv("Viral_family_RNA_1B2_consolidated.csv")
phage_1B2 <- read.csv("phage_family_DNA_1B2_consolidated.csv")

#omit empty column
vDNA_1B2$X <- NULL
vRNA_1B2$X <- NULL
phage_1B2$X <- NULL

#merge
vogue1B2 <- join(vDNA_1B2, vRNA_1B2, type="full")
vogue1B2 <- join(phage_1B2, vogue1B2, type="full")

#consolidate families together
vogue1B2a <- ddply(vogue1B2,c("Var1"),numcolwise(sum)) #includes all columns

#rename Var1
vogue1B2a <- dplyr::rename(vogue1B2a, Viral_Families = Var1)

#remove #1 (root)
vogue1B2b <- vogue1B2a[!vogue1B2a$Viral_Families == "1",]

#write to file
# write.csv(vogue1B2b, "DNA_RNA_phage_viral_families_1B2.csv")
