#re-doing for post-processed files Aug-9-16
#redone again for filtered files Sept-26-16
#redone again for omittied low detection Oct-4-16

#combine 1A, 1B, 1B2 participants into dataframe
#viral species

###############################
#Vogue1A_DNA
vogueA_52 <- read.csv(file="DNA_A_52.viralcol4_condensed.csv")
vogueA_52$VALUE <- NULL
vogueA_52 <- dplyr::rename(vogueA_52, Vogue1A.01.52 = Freq)

vogueA_59 <- read.csv(file="DNA_A_59.viralcol4_condensed.csv")
vogueA_59$VALUE <- NULL
vogueA_59 <- dplyr::rename(vogueA_59, Vogue1A.01.59 = Freq)

vogueA_61 <- read.csv(file="DNA_A_61.viralcol4_condensed.csv")
vogueA_61$VALUE <- NULL
vogueA_61 <- dplyr::rename(vogueA_61, Vogue1A.01.61 = Freq)

vogueA_62 <- read.csv(file="DNA_A_62.viralcol4_condensed.csv")
vogueA_62$VALUE <- NULL
vogueA_62 <- dplyr::rename(vogueA_62, Vogue1A.01.62 = Freq)

vogueA_64 <- read.csv(file="DNA_A_64.viralcol4_condensed.csv")
vogueA_64$VALUE <- NULL
vogueA_64 <- dplyr::rename(vogueA_64, Vogue1A.01.64 = Freq)

vogueA_65 <- read.csv(file="DNA_A_65.viralcol4_condensed.csv")
vogueA_65$VALUE <- NULL
vogueA_65 <- dplyr::rename(vogueA_65, Vogue1A.01.65 = Freq)

vogueA_68 <- read.csv(file="DNA_A_68.viralcol4_condensed.csv")
vogueA_68$VALUE <- NULL
vogueA_68 <- dplyr::rename(vogueA_68, Vogue1A.01.68 = Freq)

vogueA_69 <- read.csv(file="DNA_A_69.viralcol4_condensed.csv")
vogueA_69$VALUE <- NULL
vogueA_69 <- dplyr::rename(vogueA_69, Vogue1A.01.69 = Freq)

vogueA_70 <- read.csv(file="DNA_A_70.viralcol4_condensed.csv")
vogueA_70$VALUE <- NULL
vogueA_70 <- dplyr::rename(vogueA_70, Vogue1A.01.70 = Freq)

vogueA_71 <- read.csv(file="DNA_A_71.viralcol4_condensed.csv")
vogueA_71$VALUE <- NULL
vogueA_71 <- dplyr::rename(vogueA_71, Vogue1A.01.71 = Freq)

vogueA_74 <- read.csv(file="DNA_A_74.viralcol4_condensed.csv")
vogueA_74$VALUE <- NULL
vogueA_74 <- dplyr::rename(vogueA_74, Vogue1A.01.74 = Freq)

vogueA_75 <- read.csv(file="DNA_A_75.viralcol4_condensed.csv")
vogueA_75$VALUE <- NULL
vogueA_75 <- dplyr::rename(vogueA_75, Vogue1A.01.75 = Freq)

vogueA_76 <- read.csv(file="DNA_A_76.viralcol4_condensed.csv")
vogueA_76$VALUE <- NULL
vogueA_76 <- dplyr::rename(vogueA_76, Vogue1A.01.76 = Freq)

vogueA_77 <- read.csv(file="DNA_A_77.viralcol4_condensed.csv")
vogueA_77$VALUE <- NULL
vogueA_77 <- dplyr::rename(vogueA_77, Vogue1A.01.77 = Freq)

vogueA_78 <- read.csv(file="DNA_A_78.viralcol4_condensed.csv")
vogueA_78$VALUE <- NULL
vogueA_78 <- dplyr::rename(vogueA_78, Vogue1A.01.78 = Freq)

vogueA_81 <- read.csv(file="DNA_A_81.viralcol4_condensed.csv")
vogueA_81$VALUE <- NULL
vogueA_81 <- dplyr::rename(vogueA_81, Vogue1A.01.81 = Freq)

vogueA_84 <- read.csv(file="DNA_A_84.viralcol4_condensed.csv")
vogueA_84$VALUE <- NULL
vogueA_84 <- dplyr::rename(vogueA_84, Vogue1A.01.84 = Freq)

vogueA_85 <- read.csv(file="DNA_A_85.viralcol4_condensed.csv")
vogueA_85$VALUE <- NULL
vogueA_85 <- dplyr::rename(vogueA_85, Vogue1A.01.85 = Freq)

vogueA_92 <- read.csv(file="DNA_A_92.viralcol4_condensed.csv")
vogueA_92$VALUE <- NULL
vogueA_92 <- dplyr::rename(vogueA_92, Vogue1A.01.92 = Freq)

vogueA_101 <- read.csv(file="DNA_A_101.viralcol4_condensed.csv")
vogueA_101$VALUE <- NULL
vogueA_101 <- dplyr::rename(vogueA_101, Vogue1A.01.101 = Freq)

vogueA_106 <- read.csv(file="DNA_A_106.viralcol4_condensed.csv")
vogueA_106$VALUE <- NULL
vogueA_106 <- dplyr::rename(vogueA_106, Vogue1A.01.106 = Freq)

#################################################################
#################################################################
#################################################################
#Vogue1B_DNA
vogueB_01 <- read.csv(file="DNA_B_01.viralcol4_condensed.csv")
vogueB_01$VALUE <- NULL
vogueB_01 <- dplyr::rename(vogueB_01, Vogue1B.01.01 = Freq)

vogueB_03 <- read.csv(file="DNA_B_03.viralcol4_condensed.csv")
vogueB_03$VALUE <- NULL
vogueB_03 <- dplyr::rename(vogueB_03, Vogue1B.01.03 = Freq)

vogueB_04 <- read.csv(file="DNA_B_04.viralcol4_condensed.csv")
vogueB_04$VALUE <- NULL
vogueB_04 <- dplyr::rename(vogueB_04, Vogue1B.01.04 = Freq)

vogueB_05 <- read.csv(file="DNA_B_05.viralcol4_condensed.csv")
vogueB_05$VALUE <- NULL
vogueB_05 <- dplyr::rename(vogueB_05, Vogue1B.01.05 = Freq)

vogueB_06 <- read.csv(file="DNA_B_06.viralcol4_condensed.csv")
vogueB_06$VALUE <- NULL
vogueB_06 <- dplyr::rename(vogueB_06, Vogue1B.01.06 = Freq)

vogueB_08 <- read.csv(file="DNA_B_08.viralcol4_condensed.csv")
vogueB_08$VALUE <- NULL
vogueB_08 <- dplyr::rename(vogueB_08, Vogue1B.01.08 = Freq)

vogueB_09 <- read.csv(file="DNA_B_09.viralcol4_condensed.csv")
vogueB_09$VALUE <- NULL
vogueB_09 <- dplyr::rename(vogueB_09, Vogue1B.01.09 = Freq)

vogueB_11 <- read.csv(file="DNA_B_11.viralcol4_condensed.csv")
vogueB_11$VALUE <- NULL
vogueB_11 <- dplyr::rename(vogueB_11, Vogue1B.01.11 = Freq)

vogueB_12 <- read.csv(file="DNA_B_12.viralcol4_condensed.csv")
vogueB_12$VALUE <- NULL
vogueB_12 <- dplyr::rename(vogueB_12, Vogue1B.01.12 = Freq)

vogueB_13 <- read.csv(file="DNA_B_13.viralcol4_condensed.csv")
vogueB_13$VALUE <- NULL
vogueB_13 <- dplyr::rename(vogueB_13, Vogue1B.01.13 = Freq)

vogueB_15 <- read.csv(file="DNA_B_15.viralcol4_condensed.csv")
vogueB_15$VALUE <- NULL
vogueB_15 <- dplyr::rename(vogueB_15, Vogue1B.01.15 = Freq)

vogueB_17 <- read.csv(file="DNA_B_17.viralcol4_condensed.csv")
vogueB_17$VALUE <- NULL
vogueB_17 <- dplyr::rename(vogueB_17, Vogue1B.01.17 = Freq)

vogueB_21 <- read.csv(file="DNA_B_21.viralcol4_condensed.csv")
vogueB_21$VALUE <- NULL
vogueB_21 <- dplyr::rename(vogueB_21, Vogue1B.01.21 = Freq)

vogueB_26 <- read.csv(file="DNA_B_26.viralcol4_condensed.csv")
vogueB_26$VALUE <- NULL
vogueB_26 <- dplyr::rename(vogueB_26, Vogue1B.01.26 = Freq)

vogueB_27 <- read.csv(file="DNA_B_27.viralcol4_condensed.csv")
vogueB_27$VALUE <- NULL
vogueB_27 <- dplyr::rename(vogueB_27, Vogue1B.01.27 = Freq)

vogueB_32 <- read.csv(file="DNA_B_32.viralcol4_condensed.csv")
vogueB_32$VALUE <- NULL
vogueB_32 <- dplyr::rename(vogueB_32, Vogue1B.01.32 = Freq)

vogueB_34 <- read.csv(file="DNA_B_34.viralcol4_condensed.csv")
vogueB_34$VALUE <- NULL
vogueB_34 <- dplyr::rename(vogueB_34, Vogue1B.01.34 = Freq)

vogueB_36 <- read.csv(file="DNA_B_36.viralcol4_condensed.csv")
vogueB_36$VALUE <- NULL
vogueB_36 <- dplyr::rename(vogueB_36, Vogue1B.01.36 = Freq)

vogueB_37 <- read.csv(file="DNA_B_37.viralcol4_condensed.csv")
vogueB_37$VALUE <- NULL
vogueB_37 <- dplyr::rename(vogueB_37, Vogue1B.01.37 = Freq)

vogueB_38 <- read.csv(file="DNA_B_38.viralcol4_condensed.csv")
vogueB_38$VALUE <- NULL
vogueB_38 <- dplyr::rename(vogueB_38, Vogue1B.01.38 = Freq)

vogueB_40 <- read.csv(file="DNA_B_40.viralcol4_condensed.csv")
vogueB_40$VALUE <- NULL
vogueB_40 <- dplyr::rename(vogueB_40, Vogue1B.01.40 = Freq)

vogueB_43 <- read.csv(file="DNA_B_43.viralcol4_condensed.csv")
vogueB_43$VALUE <- NULL
vogueB_43 <- dplyr::rename(vogueB_43, Vogue1B.01.43 = Freq)

vogueB_48 <- read.csv(file="DNA_B_48.viralcol4_condensed.csv")
vogueB_48$VALUE <- NULL
vogueB_48 <- dplyr::rename(vogueB_48, Vogue1B.01.48 = Freq)

vogueB_51 <- read.csv(file="DNA_B_51.viralcol4_condensed.csv")
vogueB_51$VALUE <- NULL
vogueB_51 <- dplyr::rename(vogueB_51, Vogue1B.01.51 = Freq)

vogueB_52 <- read.csv(file="DNA_B_52.viralcol4_condensed.csv")
vogueB_52$VALUE <- NULL
vogueB_52 <- dplyr::rename(vogueB_52, Vogue1B.01.52 = Freq)

#######################################################################
#######################################################################
#######################################################################
#1B2_DNA
vogue1B2_06 <- read.csv(file="DNA_1B2_06.viralcol4_condensed.csv")
vogue1B2_06$VALUE <- NULL
vogue1B2_06 <- dplyr::rename(vogue1B2_06, Vogue1B2.01.06 = Freq)

vogue1B2_07 <- read.csv(file="DNA_1B2_07.viralcol4_condensed.csv")
vogue1B2_07$VALUE <- NULL
vogue1B2_07 <- dplyr::rename(vogue1B2_07, Vogue1B2.01.07 = Freq)

vogue1B2_08 <- read.csv(file="DNA_1B2_08.viralcol4_condensed.csv")
vogue1B2_08$VALUE <- NULL
vogue1B2_08 <- dplyr::rename(vogue1B2_08, Vogue1B2.01.08 = Freq)

vogue1B2_09 <- read.csv(file="DNA_1B2_09.viralcol4_condensed.csv")
vogue1B2_09$VALUE <- NULL
vogue1B2_09 <- dplyr::rename(vogue1B2_09, Vogue1B2.01.09 = Freq)

vogue1B2_10 <- read.csv(file="DNA_1B2_10.viralcol4_condensed.csv")
vogue1B2_10$VALUE <- NULL
vogue1B2_10 <- dplyr::rename(vogue1B2_10, Vogue1B2.01.10 = Freq)

vogue1B2_11 <- read.csv(file="DNA_1B2_11.viralcol4_condensed.csv")
vogue1B2_11$VALUE <- NULL
vogue1B2_11 <- dplyr::rename(vogue1B2_11, Vogue1B2.01.11 = Freq)

vogue1B2_12 <- read.csv(file="DNA_1B2_12.viralcol4_condensed.csv")
vogue1B2_12$VALUE <- NULL
vogue1B2_12 <- dplyr::rename(vogue1B2_12, Vogue1B2.01.12 = Freq)

vogue1B2_15 <- read.csv(file="DNA_1B2_15.viralcol4_condensed.csv")
vogue1B2_15$VALUE <- NULL
vogue1B2_15 <- dplyr::rename(vogue1B2_15, Vogue1B2.01.15 = Freq)

#####################################
#join all participants together
a <- join(vogueA_52, vogueA_59, type="full")
a <- join(a, vogueA_61, type="full")
a <- join(a, vogueA_62, type="full")
a <- join(a, vogueA_64, type="full")
a <- join(a, vogueA_65, type="full")
a <- join(a, vogueA_68, type="full")
a <- join(a, vogueA_69, type="full")
a <- join(a, vogueA_70, type="full")
a <- join(a, vogueA_71, type="full")
a <- join(a, vogueA_74, type="full")
a <- join(a, vogueA_75, type="full")
a <- join(a, vogueA_76, type="full")
a <- join(a, vogueA_77, type="full")
a <- join(a, vogueA_78, type="full")
a <- join(a, vogueA_81, type="full")
a <- join(a, vogueA_84, type="full")
a <- join(a, vogueA_85, type="full")
a <- join(a, vogueA_92, type="full")
a <- join(a, vogueA_101, type="full")
a <- join(a, vogueA_106, type="full")
###############################################
#1B
b <- join(vogueB_01, vogueB_03, type="full")
b <- join(b, vogueB_04, type="full")
b <- join(b, vogueB_05, type="full")
b <- join(b, vogueB_06, type="full")
b <- join(b, vogueB_08, type="full")
b <- join(b, vogueB_09, type="full")
b <- join(b, vogueB_11, type="full")
b <- join(b, vogueB_12, type="full")
b <- join(b, vogueB_13, type="full")
b <- join(b, vogueB_15, type="full")
b <- join(b, vogueB_17, type="full")
b <- join(b, vogueB_21, type="full")
b <- join(b, vogueB_26, type="full")
b <- join(b, vogueB_27, type="full")
b <- join(b, vogueB_32, type="full")
b <- join(b, vogueB_34, type="full")
b <- join(b, vogueB_36, type="full")
b <- join(b, vogueB_37, type="full")
b <- join(b, vogueB_38, type="full")
b <- join(b, vogueB_40, type="full")
b <- join(b, vogueB_43, type="full")
b <- join(b, vogueB_48, type="full")
b <- join(b, vogueB_51, type="full")
b <- join(b, vogueB_52, type="full")

#######################################################
#1B2
b2 <- join(vogue1B2_06, vogue1B2_07, type="full")
b2 <- join(b2, vogue1B2_08, type="full")
b2 <- join(b2, vogue1B2_09, type="full")
b2 <- join(b2, vogue1B2_10, type="full")
b2 <- join(b2, vogue1B2_11, type="full")
b2 <- join(b2, vogue1B2_12, type="full")
b2 <- join(b2, vogue1B2_15, type="full")

###########################################################
#replace NA with 0
a[is.na(a)] <- 0
b[is.na(b)] <- 0
b2[is.na(b2)] <- 0

#write dataframes to file
# write.csv(a, "Viral_species_DNA_1A.csv")
# write.csv(b, "Viral__species_DNA_1B.csv")
# write.csv(b2, "Viral_species_DNA_1B2.csv")

#join all 3 together and write to file
total <- join(a, b, type="full")
total <- join(total, b2, type="full")
total[is.na(total)] <- 0
# write.csv(total, "Viral_species_DNA.csv")
