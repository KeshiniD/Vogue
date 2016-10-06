#combining NTCs together
#for omittied low detection Oct-6-16
#combine 1A, 1B, 1B2 participants into dataframe

###############################
#DNA_Viruses
DNA_NTC_A9 <- read.csv(file="DNA_NTC_A9.viralcol4_condensed.csv")
DNA_NTC_A9$VALUE <- NULL
DNA_NTC_A9 <- dplyr::rename(DNA_NTC_A9, DNA_NTC_A9 = Freq)

DNA_NTC_E12 <- read.csv(file="DNA_NTC_E12.viralcol4_condensed.csv")
DNA_NTC_E12$VALUE <- NULL
DNA_NTC_E12 <- dplyr::rename(DNA_NTC_E12, DNA_NTC_E12 = Freq)

DNA_KlenNeg_A11 <- read.csv(file="DNA_KlenNeg_A11.viralcol4_condensed.csv")
DNA_KlenNeg_A11$VALUE <- NULL
DNA_KlenNeg_A11 <- dplyr::rename(DNA_KlenNeg_A11, DNA_KlenNeg_A11 = Freq)

DNA_KlenNeg_A12 <- read.csv(file="DNA_KlenNeg_A12.viralcol4_condensed.csv")
DNA_KlenNeg_A12$VALUE <- NULL
DNA_KlenNeg_A12 <- dplyr::rename(DNA_KlenNeg_A12, DNA_KlenNeg_A12 = Freq)

DNA_Adeno <- read.csv(file="DNA_Adeno.viralcol4_condensed.csv")
DNA_Adeno$VALUE <- NULL
DNA_Adeno <- dplyr::rename(DNA_Adeno, DNA_Adeno = Freq)

###########################################################################
#DNA_phage
DNA_NTC_A9_p <- read.csv(file="DNA_NTC_A9.phagecol4_condensed.csv")
DNA_NTC_A9_p$VALUE <- NULL
DNA_NTC_A9_p <- dplyr::rename(DNA_NTC_A9_p, DNA_NTC_A9 = Freq)

DNA_NTC_E12_p <- read.csv(file="DNA_NTC_E12.phagecol4_condensed.csv")
DNA_NTC_E12_p$VALUE <- NULL
DNA_NTC_E12_p <- dplyr::rename(DNA_NTC_E12_p, DNA_NTC_E12 = Freq)

DNA_Adeno_p <- read.csv(file="DNA_Adeno.phagecol4_condensed.csv")
DNA_Adeno_p <- dplyr::rename(DNA_Adeno_p, DNA_Adeno = a, Var1 = VALUE)

##############################################################################
#RNA_Viruses
RNA_NTC_B9 <- read.csv(file="RNA_NTC_B9.viralcol4_condensed.csv")
RNA_NTC_B9$VALUE <- NULL
RNA_NTC_B9 <- dplyr::rename(RNA_NTC_B9, RNA_NTC_B9 = Freq)

RNA_NTC_B11 <- read.csv(file="RNA_NTC_B11.viralcol4_condensed.csv")
RNA_NTC_B11$VALUE <- NULL
RNA_NTC_B11 <- dplyr::rename(RNA_NTC_B11, RNA_NTC_B11 = Freq)

RNA_KlenNeg_B12 <- read.csv(file="RNA_KlenNeg_B12.viralcol4_condensed.csv")
RNA_KlenNeg_B12$VALUE <- NULL
RNA_KlenNeg_B12 <- dplyr::rename(RNA_KlenNeg_B12, RNA_KlenNeg_B12 = Freq)

RNA_Entero <- read.csv(file="RNA_Entero.viralcol4_condensed.csv")
RNA_Entero$VALUE <- NULL
RNA_Entero <- dplyr::rename(RNA_Entero, RNA_Entero = Freq)

#####################################
#join all together
# a <- join(DNA_Adeno, DNA_Adeno_p, type="full")
# a <- join(a, DNA_KlenNeg_A11, type="full")
# a <- join(a, DNA_KlenNeg_A12, type="full")
a <- join(a, DNA_NTC_A9, type="full")
a <- join(a, DNA_NTC_A9_p, type="full")
a <- join(a, DNA_NTC_E12, type="full")
a <- join(a, DNA_NTC_E12_p, type="full")
a <- join(a, RNA_NTC_B9, type="full")
a <- join(a, RNA_NTC_B11, type="full")
a <- join(a, RNA_KlenNeg_B12, type="full")
a <- join(a, RNA_Entero, type="full")

###########################################################
#replace NA with 0
a[is.na(a)] <- 0

#write dataframes to file
# write.csv(a, "DNA_RNA_phage_viral_species_NTCs_PosCtrl.csv")
