data <- read.csv(file.path("1B2data_groups.csv"))

#change headers
data <- dplyr::rename(data, Species = Group, Vogue1B2.01.26 = X1B2_01_26, 
                      Vogue1B2.01.35 = X1B2_01_35, Vogue1B2.01.37 = X1B2_01_37, 
                      Vogue1B2.01.38 = X1B2_01_38, Vogue1B2.01.50 = X1B2_01_50, 
                      Vogue1B2.01.52 = X1B2_01_52, Vogue1B2.01.56 = X1B2_01_56, 
                      Vogue1B2.01.58 = X1B2_01_58, Vogue1B2.01.61 = X1B2_01_61, 
                      Vogue1B2.01.62 = X1B2_01_62, Vogue1B2.01.63 = X1B2_01_63, 
                      Vogue1B2.01.64 = X1B2_01_64)
data2 <- data %>%
  select(Species, Vogue1B2.01.01, Vogue1B2.01.06, Vogue1B2.01.07, Vogue1B2.01.08, 
         Vogue1B2.01.09, Vogue1B2.01.10, Vogue1B2.01.11, Vogue1B2.01.12, 
         Vogue1B2.01.15, Vogue1B2.01.19, Vogue1B2.01.21, Vogue1B2.01.23, 
         Vogue1B2.01.26, Vogue1B2.01.28, Vogue1B2.01.29, Vogue1B2.01.35, 
         Vogue1B2.01.37, Vogue1B2.01.38, Vogue1B2.01.50, Vogue1B2.01.52, 
         Vogue1B2.01.56, Vogue1B2.01.58, Vogue1B2.01.61, Vogue1B2.01.62, 
         Vogue1B2.01.63, Vogue1B2.01.64)

#transpose data and keep as data frame, swap rows and cols
rownames(data2) <- data2[,1]
data2[,1] <- NULL

data3 <- as.data.frame(t(data2))

#bac counts
data2 <-
  gather(data, key = 'Bacteria', value = 'Counts', Actinobacteria sp., Lactobacillus.crispatus, Lactobacillus.iners, Lactobacillus.gasseri, 
         Lactobacillus.jensenii, Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.A, 
         Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.D, 
         Megasphaera.sp.genomosp.type.1, Escherichia.coli, Prevotella.timonensis, Clostridia.sp.BVAB2, 
         Clostridium.genomosp.BVAB3, Atopobium.vaginae, Anaerobes, Other.Clostridia, 
         Other.Bacteroidetes, Other.Proteobacteria, Other.Actinobacteria, Other.Firmicutes, Other)

