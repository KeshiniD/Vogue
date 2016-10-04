total <- read.csv("DNA_RNA_phage_viral_species_all.csv")
total$X <- NULL
total[is.na(total)] <- 0
total$Viral_Species <- factor(total$Viral_Species)

sum <- total[2:55] #omit the first Viral_Species column
rsum <- data.frame(rowSums(sum)) #sum of the rows

#merge back with Viral_Species
total2 <- cbind(total, rsum)
#rename
total3 <- dplyr::rename(total2, Total_Reads = rowSums.sum.)

#remove viral_species(ncbicode) which have less than 3 reads
newdata <- total3[ which(total3$Total_Reads > 2), ]

#write to file
# write.csv(newdata, "DNA_RNA_phage_viral_species_all_v2.csv")

#######################################################################################
# do not omit this way; species could be present in high amounts in a dif cohort
# #1A, 1B, 1B2
# vogueA <- read.csv("DNA_RNA_phage_viral_species_1A.csv")
# vogueB <- read.csv("DNA_RNA_phage_viral_species_1B.csv")
# vogue1B2 <- read.csv("DNA_RNA_phage_viral_species_1B2.csv")
# 
# #####
# #1A
# vogueA$X <- NULL
# vogueA[is.na(vogueA)] <- 0
# vogueA$Viral_Species <- factor(vogueA$Viral_Species)
# 
# sum <- vogueA[2:22] #omit the first Viral_Species column
# rsum <- data.frame(rowSums(sum)) #sum of the rows
# 
# #merge back with Viral_Species
# vogueA2 <- cbind(vogueA, rsum)
# #rename
# vogueA3 <- dplyr::rename(vogueA2, Total_Reads = rowSums.sum.)
# 
# #remove viral_species(ncbicode) which have less than 3 reads
# newdataA <- vogueA3[ which(vogueA3$Total_Reads > 2), ]
# 
# #write to file
# # write.csv(newdataA, "DNA_RNA_phage_viral_species_1A_v2.csv")
# 
# #############
# #1B
# vogueB$X <- NULL
# vogueB[is.na(vogueB)] <- 0
# vogueB$Viral_Species <- factor(vogueB$Viral_Species)
# 
# sum <- vogueB[2:26] #omit the first Viral_Species column
# rsum <- data.frame(rowSums(sum)) #sum of the rows
# 
# #merge back with Viral_Species
# vogueB2 <- cbind(vogueB, rsum)
# #rename
# vogueB3 <- dplyr::rename(vogueB2, Total_Reads = rowSums.sum.)
# 
# #remove viral_species(ncbicode) which have less than 3 reads
# newdataB <- vogueB3[ which(vogueB3$Total_Reads > 2), ]
# 
# #write to file
# # write.csv(newdataB, "DNA_RNA_phage_viral_species_1B_v2.csv")
# 
# ##################
# #1B2
# vogue1B2$X <- NULL
# vogue1B2[is.na(vogue1B2)] <- 0
# vogue1B2$Viral_Species <- factor(vogue1B2$Viral_Species)
# 
# sum <- vogue1B2[2:9] #omit the first Viral_Species column
# rsum <- data.frame(rowSums(sum)) #sum of the rows
# 
# #merge back with Viral_Species
# vogue1B22 <- cbind(vogue1B2, rsum)
# #rename
# vogue1B23 <- dplyr::rename(vogue1B22, Total_Reads = rowSums.sum.)
# 
# #remove viral_species(ncbicode) which have less than 3 reads
# newdataB2 <- vogue1B23[ which(vogue1B23$Total_Reads > 2), ]
# 
# #write to file
# # write.csv(newdataB2, "DNA_RNA_phage_viral_species_1B2_v2.csv")

#subset for three cohorts
newdatasubsetA <- newdata %>% 
  select(Viral_Species, Vogue1A.01.52, Vogue1A.01.59, Vogue1A.01.61, 
         Vogue1A.01.62, Vogue1A.01.64, Vogue1A.01.65, Vogue1A.01.68, Vogue1A.01.69, 
         Vogue1A.01.70, Vogue1A.01.71, Vogue1A.01.74, Vogue1A.01.75,Vogue1A.01.76, 
         Vogue1A.01.77, Vogue1A.01.78,Vogue1A.01.81,Vogue1A.01.84,Vogue1A.01.85, 
         Vogue1A.01.92, Vogue1A.01.101, Vogue1A.01.106)

newdatasubsetB <- newdata %>%
  select(Viral_Species, Vogue1B.01.01, Vogue1B.01.03, Vogue1B.01.04, 
        Vogue1B.01.05, Vogue1B.01.06, Vogue1B.01.08, Vogue1B.01.09, Vogue1B.01.11, 
        Vogue1B.01.12, Vogue1B.01.13, Vogue1B.01.15, Vogue1B.01.17, Vogue1B.01.21, 
        Vogue1B.01.26, Vogue1B.01.27, Vogue1B.01.32, Vogue1B.01.34, 
        Vogue1B.01.36, Vogue1B.01.37, Vogue1B.01.38, Vogue1B.01.40, Vogue1B.01.43, 
        Vogue1B.01.48, Vogue1B.01.51, Vogue1B.01.52)

newdatasubsetB2 <- newdata %>% 
  select(Viral_Species, Vogue1B2.01.06, Vogue1B2.01.07, Vogue1B2.01.08,
         Vogue1B2.01.09, Vogue1B2.01.10, Vogue1B2.01.11, Vogue1B2.01.12, Vogue1B2.01.15)

#omit those with low detection and write to file
#1A, 1B, 1B2
vogueA <- newdatasubsetA
vogueB <- newdatasubsetB
vogue1B2 <- newdatasubsetB2

#####
#1A
vogueA$X <- NULL
vogueA[is.na(vogueA)] <- 0
vogueA$Viral_Species <- factor(vogueA$Viral_Species)

sum <- vogueA[2:22] #omit the first Viral_Species column
rsum <- data.frame(rowSums(sum)) #sum of the rows

#merge back with Viral_Species
vogueA2 <- cbind(vogueA, rsum)
#rename
vogueA3 <- dplyr::rename(vogueA2, Total_Reads = rowSums.sum.)

#remove viral_species(ncbicode) which have less than 3 reads
newdataA <- vogueA3[ which(vogueA3$Total_Reads > 2), ]

#write to file
# write.csv(newdataA, "DNA_RNA_phage_viral_species_1A_v2.csv")

#############
#1B
vogueB$X <- NULL
vogueB[is.na(vogueB)] <- 0
vogueB$Viral_Species <- factor(vogueB$Viral_Species)

sum <- vogueB[2:26] #omit the first Viral_Species column
rsum <- data.frame(rowSums(sum)) #sum of the rows

#merge back with Viral_Species
vogueB2 <- cbind(vogueB, rsum)
#rename
vogueB3 <- dplyr::rename(vogueB2, Total_Reads = rowSums.sum.)

#remove viral_species(ncbicode) which have less than 3 reads
newdataB <- vogueB3[ which(vogueB3$Total_Reads > 2), ]

#write to file
# write.csv(newdataB, "DNA_RNA_phage_viral_species_1B_v2.csv")

##################
#1B2
vogue1B2$X <- NULL
vogue1B2[is.na(vogue1B2)] <- 0
vogue1B2$Viral_Species <- factor(vogue1B2$Viral_Species)

sum <- vogue1B2[2:9] #omit the first Viral_Species column
rsum <- data.frame(rowSums(sum)) #sum of the rows

#merge back with Viral_Species
vogue1B22 <- cbind(vogue1B2, rsum)
#rename
vogue1B23 <- dplyr::rename(vogue1B22, Total_Reads = rowSums.sum.)

#remove viral_species(ncbicode) which have less than 3 reads
newdataB2 <- vogue1B23[ which(vogue1B23$Total_Reads > 2), ]

#write to file
# write.csv(newdataB2, "DNA_RNA_phage_viral_species_1B2_v2.csv")
