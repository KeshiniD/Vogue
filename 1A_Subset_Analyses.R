#load packages
library(pwr)

##Power Calculations
pwr.t2n.test(n1 = 26, n2=104 , sig.level = 0.05, power = 0.8)
# t test power calculation 
#n1 = 26
#n2 = 104
#d = 0.6189457 (can detect medium differences)
#sig.level = 0.05
#power = 0.8
#alternative = two.sided

pwr.t2n.test(n1 = 26, n2=300 , sig.level = 0.05, power = 0.8)#changed n2
#t test power calculation 
#n1 = 26
#n2 = 300
#d = 0.5744546
#sig.level = 0.05
#power = 0.8
#alternative = two.sided

pwr.2p2n.test(n1 = 26, n2=104 , sig.level = 0.05, power = 0.8)
#difference of proportion power calculation for binomial distribution (arcsine transformation) 
#h = 0.6142887 (can detect medium differences)
#n1 = 26
#n2 = 104
#sig.level = 0.05
#power = 0.8
#alternative = two.sided
#NOTE: different sample sizes

pwr.2p2n.test(n1 = 26, n2=300 , sig.level = 0.05, power = 0.8)#changed n2
#difference of proportion power calculation for binomial distribution (arcsine transformation) 
#h = 0.5727499
#n1 = 26
#n2 = 300
#sig.level = 0.05
#power = 0.8
#alternative = two.sided
#NOTE: different sample sizes

##Diversity Indices
#load packages
library(vegan)
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)
library(entropart)
library(epitools)

#load dataset
data <- read.csv(file.path("data1A_1B2.csv"))

#column into row labels
data2 <- data[,-1]
rownames(data2) <- data[,1]

#select subset of participants wish to analyze
data2 <- data2 %>%
  select(Vogue1A.01.003, Vogue1A.01.007, Vogue1A.01.012, 
         Vogue1A.01.016, Vogue1A.01.017, Vogue1A.01.019, Vogue1A.01.020, 
         Vogue1A.01.022, Vogue1A.01.023, Vogue1A.01.024, Vogue1A.01.025, 
         Vogue1A.01.026, Vogue1A.01.029, Vogue1A.01.030, Vogue1A.01.033, 
         Vogue1A.01.034, Vogue1A.01.036, Vogue1A.01.041, Vogue1A.01.042, 
         Vogue1A.01.046, Vogue1A.01.049, Vogue1A.01.054, Vogue1A.01.060, 
         Vogue1A.01.061, Vogue1A.01.063, Vogue1A.01.066, Vogue1A.01.067, 
         Vogue1A.01.072, Vogue1A.01.073, Vogue1A.01.077, Vogue1A.01.078, 
         Vogue1A.01.079, Vogue1A.01.082, Vogue1A.01.085, Vogue1A.01.089, 
         Vogue1A.01.090, Vogue1A.01.092, Vogue1A.01.095, Vogue1A.01.097, 
         Vogue1A.01.099, Vogue1A.01.100, Vogue1A.01.107, Vogue1A.01.112, 
         Vogue1A.01.113, Vogue1A.01.114, Vogue1A.01.118, Vogue1A.01.123, 
         Vogue1A.01.125, Vogue1A.01.126, Vogue1A.01.127, Vogue1A.01.128, 
         Vogue1A.01.130, Vogue1A.01.134, Vogue1A.01.135, Vogue1A.01.137, 
         Vogue1A.01.139, Vogue1A.01.140, Vogue1A.01.142, Vogue1A.01.145, 
         Vogue1A.01.146, Vogue1A.01.147, Vogue1A.01.150, Vogue1A.01.152, 
         Vogue1A.01.156, Vogue1A.01.157, Vogue1A.01.161, Vogue1A.01.162, 
         Vogue1A.01.163, Vogue1A.01.166, Vogue1A.01.169, Vogue1A.01.170, 
         Vogue1A.01.172, Vogue1A.01.173, Vogue1A.01.174, Vogue1A.01.175, 
         Vogue1A.01.184, Vogue1A.01.186, Vogue1A.01.187, Vogue1A.01.191, 
         Vogue1A.01.192, Vogue1A.01.197, Vogue1A.01.198, Vogue1A.01.200, 
         Vogue1A.01.201, Vogue1A.01.203, Vogue1A.01.205, Vogue1A.01.207, 
         Vogue1A.01.208, Vogue1A.01.211, Vogue1A.01.212, Vogue1A.01.213, 
         Vogue1A.01.214, Vogue1A.01.215, Vogue1A.01.216, Vogue1A.01.217, 
         Vogue1A.01.218, Vogue1A.01.219, Vogue1A.01.224, Vogue1A.01.227, 
         Vogue1A.01.231, Vogue1A.01.233, Vogue1A.01.236, Vogue1A.01.237, 
         Vogue1A.01.238)

#transpose dataset
data2 <- as.data.frame(t(data2))


##Shannon's Diversity for Individuals
H <- diversity(data2) 
H <- as.data.frame(H)
View(H)

#row names into column
H <- add_rownames(H, "VALUE")
#rename headers
H <- dplyr::rename(H, ShannonsDiversity = H, Participants = VALUE)

#write data to file 
write.table(H, "1A_subset_individual_diversity.csv", sep = ",", row.names = FALSE, quote = FALSE)
div <- read.csv(file.path("1A_subset_individual_diversity.csv"))

#SD cohort
#first need to make new table with total counts for each bacterial species
#load dataset
data <- read.csv(file.path("data1A_1B2.csv"))

#column into row labels
data2 <- data[,-1]
rownames(data2) <- data[,1]

#select subset of participants wish to analyze
data2 <- data2 %>%
  select(Vogue1A.01.003, Vogue1A.01.007, Vogue1A.01.012, 
         Vogue1A.01.016, Vogue1A.01.017, Vogue1A.01.019, Vogue1A.01.020, 
         Vogue1A.01.022, Vogue1A.01.023, Vogue1A.01.024, Vogue1A.01.025, 
         Vogue1A.01.026, Vogue1A.01.029, Vogue1A.01.030, Vogue1A.01.033, 
         Vogue1A.01.034, Vogue1A.01.036, Vogue1A.01.041, Vogue1A.01.042, 
         Vogue1A.01.046, Vogue1A.01.049, Vogue1A.01.054, Vogue1A.01.060, 
         Vogue1A.01.061, Vogue1A.01.063, Vogue1A.01.066, Vogue1A.01.067, 
         Vogue1A.01.072, Vogue1A.01.073, Vogue1A.01.077, Vogue1A.01.078, 
         Vogue1A.01.079, Vogue1A.01.082, Vogue1A.01.085, Vogue1A.01.089, 
         Vogue1A.01.090, Vogue1A.01.092, Vogue1A.01.095, Vogue1A.01.097, 
         Vogue1A.01.099, Vogue1A.01.100, Vogue1A.01.107, Vogue1A.01.112, 
         Vogue1A.01.113, Vogue1A.01.114, Vogue1A.01.118, Vogue1A.01.123, 
         Vogue1A.01.125, Vogue1A.01.126, Vogue1A.01.127, Vogue1A.01.128, 
         Vogue1A.01.130, Vogue1A.01.134, Vogue1A.01.135, Vogue1A.01.137, 
         Vogue1A.01.139, Vogue1A.01.140, Vogue1A.01.142, Vogue1A.01.145, 
         Vogue1A.01.146, Vogue1A.01.147, Vogue1A.01.150, Vogue1A.01.152, 
         Vogue1A.01.156, Vogue1A.01.157, Vogue1A.01.161, Vogue1A.01.162, 
         Vogue1A.01.163, Vogue1A.01.166, Vogue1A.01.169, Vogue1A.01.170, 
         Vogue1A.01.172, Vogue1A.01.173, Vogue1A.01.174, Vogue1A.01.175, 
         Vogue1A.01.184, Vogue1A.01.186, Vogue1A.01.187, Vogue1A.01.191, 
         Vogue1A.01.192, Vogue1A.01.197, Vogue1A.01.198, Vogue1A.01.200, 
         Vogue1A.01.201, Vogue1A.01.203, Vogue1A.01.205, Vogue1A.01.207, 
         Vogue1A.01.208, Vogue1A.01.211, Vogue1A.01.212, Vogue1A.01.213, 
         Vogue1A.01.214, Vogue1A.01.215, Vogue1A.01.216, Vogue1A.01.217, 
         Vogue1A.01.218, Vogue1A.01.219, Vogue1A.01.224, Vogue1A.01.227, 
         Vogue1A.01.231, Vogue1A.01.233, Vogue1A.01.236, Vogue1A.01.237, 
         Vogue1A.01.238)

#transpose dataset
data2 <- as.data.frame(t(data2))
#row into column
data2 <- add_rownames(data2, "Participants")

#write data to file 
write.table(data2, "1A_temp.csv", sep = ",", row.names = FALSE, quote = FALSE)
data2 <- read.csv(file.path("1A_temp.csv"))


#bac counts
data3 <-
  gather(data2, key = 'Bacteria', value = 'Counts', Actinobacteria.sp., 
         Alloscardovia.omnicolens, Atopobium.vaginae, 
         Bifidobacterium.breve, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Enterococcus.rattus, 
         Escherichia.coli, Eukaryote, Gardnerella.vaginalis.Group.A, 
         Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.C, 
         Gardnerella.vaginalis.Group.D, Klebsiella.pneumoniae, 
         Lactobacillus.crispatus, Lactobacillus.gasseri, 
         Lactobacillus.iners, Lactobacillus.jensenii, 
         Megasphaera.sp..genomosp..type.1, Other.Actinobacteria, 
         Other.Bacteria, Other.Bacteroidetes, Other.Bifidobacterium, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus, 
         Porphyromonas.uenonis, Prevotella.amnii, Prevotella.timonensis, 
         Pseudomonas.putida, Streptococcus.devriesei, Variovorax.paradoxus)

#then use in diversity
H2 <- data3 %>% 
  select (Bacteria, Counts) %>% #WORKED!!!! for cohort
  group_by(Bacteria) %>%
  summarize(TotalCounts = sum(Counts)) %>%
  select (TotalCounts) #stay in for diversity,can remove to see bac
F2 <- diversity(H2)
View(F2)

#remove the absent bacterial species and see if this alters SD
H2 <- H2[-c(1,8,14), ]
F3 <- diversity(H2)
View(F3) #exactly the same
F3 <- as.data.frame(F3) #data.frame

#rename headers
F3 <- dplyr::rename(F3, ShannonsDiversity = F3)

#write data to file 
write.table(F3, "1A_subset_cohort_diversity.csv", sep = ",", row.names = FALSE, quote = FALSE)
cdiv <- read.csv(file.path("1A_subset_cohort_diversity.csv"))


##Pielou's eveness
#remove participants
data2 <- data2[-1 ]
J<- H/log(specnumber(data2)) # for individuals
J <- as.data.frame(J)
#row into column
J <- add_rownames(J, "Participants")
#rename headers
#rename headers
J <- dplyr::rename(J, PielousEveness = H)
View(J)

#write data to file (altered headings and called back)
write.table(J, "1A_subset_individual_Pielou.csv", sep = ",", row.names = FALSE, quote = FALSE)
Piel <- read.csv(file.path("1A_subset_individual_Pielou.csv"))

J2 <- F2/log(specnumber(H2, MARGIN = 2)) #not working for entire cohort
#NOV-12-2015: fixed above code; margin set to 2; finds frequencies of species
#specnumber want number of species and we can manually enter 21 to get J2
#fixed it with below code, and can be used for diversity
J2 <- as.data.frame(J2)
J2 <- dplyr::rename(J2, PielousEveness = J2)
View(J2)

#NOV-12-2015: do not need below code
# want to sum up all bacteria in one row, with bacteria as columns
# can then use this in specnumber() and then pielou for cohort
h4 <- colSums(H2)
H5 <-  t(h4)
J2 <- F2/log(H5) 
View(J2)

#write data to file (altered headings and called back)
write.table(J2, "1A_subset_cohort_Pielou.csv", sep = ",", row.names = FALSE, quote = FALSE)
cPiel <- read.csv(file.path("1A_subset_cohort_Pielou.csv"))


##Good's Coverage
## need entropart package

#example
data(Paracou618) 
Ns <- Paracou618.MC$Ns
Coverage(Ns)

#apply to own data
#same as above
#bac counts
data3 <-
  gather(data2, key = 'Bacteria', value = 'Counts', Actinobacteria.sp., 
         Alloscardovia.omnicolens, Atopobium.vaginae, 
         Bifidobacterium.breve, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Enterococcus.rattus, 
         Escherichia.coli, Eukaryote, Gardnerella.vaginalis.Group.A, 
         Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.C, 
         Gardnerella.vaginalis.Group.D, Klebsiella.pneumoniae, 
         Lactobacillus.crispatus, Lactobacillus.gasseri, 
         Lactobacillus.iners, Lactobacillus.jensenii, 
         Megasphaera.sp..genomosp..type.1, Other.Actinobacteria, 
         Other.Bacteria, Other.Bacteroidetes, Other.Bifidobacterium, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus, 
         Porphyromonas.uenonis, Prevotella.amnii, Prevotella.timonensis, 
         Pseudomonas.putida, Streptococcus.devriesei, Variovorax.paradoxus)

#then use in diversity
data4 <- data3 %>% 
  select (Bacteria, Counts) %>% #WORKED!!!! for cohort
  group_by(Bacteria) %>%
  summarize(TotalCounts = sum(Counts)) %>%
  select (TotalCounts)

Ns <- data4$TotalCounts
Coverage(Ns, Estimator = Turing) #Ns has to be numeric vector
Coverage(Ns) # is value correct; value could be for entire cohort?


#Individuals
#may have to do separate manually and calculate each
#this subsets data based on participants :)
newdata <- data3[ which(data3$Participants=='Vogue1A.01.003'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
a <- as.data.frame(Coverage(Ns, Estimator = "Turing"))
#insert each participant for coverage

data3 %>%
  group_by(Participants) %>%
  summarise(a = Coverage(Counts, Estimator = "Turing"))

#Vogue 01-007
newdata <- data3[ which(data3$Participants=='Vogue1A.01.007'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
b <- as.data.frame(Coverage(Ns, Estimator = "Turing"))

#Vogue 01-012
newdata <- data3[ which(data3$Participants=='Vogue1A.01.012'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
c <- as.data.frame(Coverage(Ns, Estimator = "Turing"))

#Vogue 01-016
newdata <- data3[ which(data3$Participants=='Vogue1A.01.016'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
d <- as.data.frame(Coverage(Ns, Estimator = "Turing"))

#Vogue1A.01.017
newdata <- data3[ which(data3$Participants=='Vogue1A.01.017'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
e <- as.data.frame(Coverage(Ns, Estimator = "Turing"))



# [6] Vogue1A.01.019 Vogue1A.01.020 Vogue1A.01.022 Vogue1A.01.023 Vogue1A.01.024
# [11] Vogue1A.01.025 Vogue1A.01.026 Vogue1A.01.029 Vogue1A.01.030 Vogue1A.01.033
# [16] Vogue1A.01.034 Vogue1A.01.036 Vogue1A.01.041 Vogue1A.01.042 Vogue1A.01.046
# [21] Vogue1A.01.049 Vogue1A.01.054 Vogue1A.01.060 Vogue1A.01.061 Vogue1A.01.063
# [26] Vogue1A.01.066 Vogue1A.01.067 Vogue1A.01.072 Vogue1A.01.073 Vogue1A.01.077
# [31] Vogue1A.01.078 Vogue1A.01.079 Vogue1A.01.082 Vogue1A.01.085 Vogue1A.01.089
# [36] Vogue1A.01.090 Vogue1A.01.092 Vogue1A.01.095 Vogue1A.01.097 Vogue1A.01.099
# [41] Vogue1A.01.100 Vogue1A.01.107 Vogue1A.01.112 Vogue1A.01.113 Vogue1A.01.114
# [46] Vogue1A.01.118 Vogue1A.01.123 Vogue1A.01.125 Vogue1A.01.126 Vogue1A.01.127
# [51] Vogue1A.01.128 Vogue1A.01.130 Vogue1A.01.134 Vogue1A.01.135 Vogue1A.01.137
# [56] Vogue1A.01.139 Vogue1A.01.140 Vogue1A.01.142 Vogue1A.01.145 Vogue1A.01.146
# [61] Vogue1A.01.147 Vogue1A.01.150 Vogue1A.01.152 Vogue1A.01.156 Vogue1A.01.157
# [66] Vogue1A.01.161 Vogue1A.01.162 Vogue1A.01.163 Vogue1A.01.166 Vogue1A.01.169
# [71] Vogue1A.01.170 Vogue1A.01.172 Vogue1A.01.173 Vogue1A.01.174 Vogue1A.01.175
# [76] Vogue1A.01.184 Vogue1A.01.186 Vogue1A.01.187 Vogue1A.01.191 Vogue1A.01.192
# [81] Vogue1A.01.197 Vogue1A.01.198 Vogue1A.01.200 Vogue1A.01.201 Vogue1A.01.203
# [86] Vogue1A.01.205 Vogue1A.01.207 Vogue1A.01.208 Vogue1A.01.211 Vogue1A.01.212
# [91] Vogue1A.01.213 Vogue1A.01.214 Vogue1A.01.215 Vogue1A.01.216 Vogue1A.01.217
# [96] Vogue1A.01.218 Vogue1A.01.219 Vogue1A.01.224 Vogue1A.01.227 Vogue1A.01.231
# [101] Vogue1A.01.233 Vogue1A.01.236 Vogue1A.01.237 Vogue1A.01.238

#merge
#list.of.data.frames <- cbind()
#edit headers
# list.of.data.frames <- dplyr::rename(list.of.data.frames, Vogue1B2.01.01 = a, 
#                                      Vogue1B2.01.06 = b, Vogue1B2.01.07 = c, 
#                                      Vogue1B2.01.08 = d, Vogue1B2.01.09 = e, 
#                                      Vogue1B2.01.10 = f, Vogue1B2.01.11 = g, 
#                                      Vogue1B2.01.12 = h, Vogue1B2.01.15 = i, 
#                                      Vogue1B2.01.19 = j, Vogue1B2.01.21 = k, 
#                                      Vogue1B2.01.23 = l, Vogue1B2.01.26 = m, 
#                                      Vogue1B2.01.28 = n, Vogue1B2.01.29 = o, 
#                                      Vogue1B2.01.35 = p, Vogue1B2.01.37 = q, 
#                                      Vogue1B2.01.38 = r, Vogue1B2.01.50 = s, 
#                                      Vogue1B2.01.52 = t, Vogue1B2.01.56 = u, 
#                                      Vogue1B2.01.58 = v, Vogue1B2.01.61 = w, 
#                                      Vogue1B2.01.62 = x, Vogue1B2.01.63 = y, 
#                                      Vogue1B2.01.64 = z) 
#when transpose and write to file lose 'Participant' IDs
#list.of.data.frames <- as.data.frame(t(list.of.data.frames))

#write
#write.table(list.of.data.frames, "1B2_individual_Good.csv", sep = ",", row.names = FALSE, quote = FALSE)
#Good <- read.csv(file.path("1B2_individual_Good.csv"))
#df <- t(Good) #transposed
#write.table(df, "1B2_individual_Good_transposed.csv", sep = ",", row.names = FALSE, quote = FALSE)


#Chao estimator
#cohort
#data2 step before adding row names
d <- diversityresult(data2, index = 'chao')
View(d)

#write to file
#write.csv(d, "1A_Chao_cohort.csv")

#individuals
library(fossil)
#call data 
#want data now with row name

#separate manually and calculate each
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.01'), ]
newdata[,1] <- NULL
a <- chao1(newdata)
#insert each participant for coverage

#turn all variables into data.frame, join data.frames and write into file
#merge
list.of.data.frames <- cbind()
#edit headers
list.of.data.frames <- dplyr::rename(list.of.data.frames, Vogue1B2.01.01 = a, 
                                     Vogue1B2.01.06 = b, Vogue1B2.01.07 = c, 
                                     Vogue1B2.01.08 = d, Vogue1B2.01.09 = e, 
                                     Vogue1B2.01.10 = f, Vogue1B2.01.11 = g, 
                                     Vogue1B2.01.12 = h, Vogue1B2.01.15 = i, 
                                     Vogue1B2.01.19 = j, Vogue1B2.01.21 = k, 
                                     Vogue1B2.01.23 = l, Vogue1B2.01.26 = m, 
                                     Vogue1B2.01.28 = n, Vogue1B2.01.29 = o, 
                                     Vogue1B2.01.35 = p, Vogue1B2.01.37 = q, 
                                     Vogue1B2.01.38 = r, Vogue1B2.01.50 = s, 
                                     Vogue1B2.01.52 = t, Vogue1B2.01.56 = u, 
                                     Vogue1B2.01.58 = v, Vogue1B2.01.61 = w, 
                                     Vogue1B2.01.62 = x, Vogue1B2.01.63 = y, 
                                     Vogue1B2.01.64 = z) 
list.of.data.frames2 <- as.data.frame(t(list.of.data.frames))

#write to file
#write.csv(list.of.data.frames2, "1A_Chao_individual.csv")

#merge shannon, peilous, rarefaction, good and chao1
a <- read.csv(file.path("1A_subset_individual_Pielou.csv"))
b <- read.csv(file.path("1B2richness_individual.csv")) #rarefraction
c <- read.csv(file.path("1A_subset_individual_diversity.csv"))
d <- read.csv(file.path("1A_Chao_individual.csv"))
e <- read.csv(file.path("1A_individual_Good_transposed.csv"))
b$X <- NULL #remove random empty column
d <- dplyr::rename(d, Participants = X, Chao1 = V1) 
e <- dplyr::rename(e, Participants = X, GoodsCoverageEstimator = V1) 

#merge three folders together
diversity <- cbind(a,b,c,d,e)
#remove duplicate columns
diversity2 <- diversity[ -c(3, 5, 7, 9) ]

#write
#write.csv(diversity2, "1A_individual_all_diversity.csv")

##Rarefraction
#call for data
#want all bacteria (in columns), no participants
data <- read.csv(file.path("1B2.csv"))

bac <- data %>%
  select(Lactobacillus.crispatus, Lactobacillus.gasseri, 
         Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, Eukaryote, 
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)

#set sample to integer (should be smaller than sample size)
#should probabaly set sample size to min(rowSums(bac))?
rarefy <- rarefy(bac, sample=min(rowSums(bac))) 

#write individual species richness into file
rarefy <- as.data.frame(rarefy)
#write.csv(rarefy, "1Arichness_individual.csv")

#richness for cohort
#already have bac subset data
#first need to make new table with total counts for each bacterial species
#bac counts
bac2 <-
  gather(bac, key = 'Bacteria', value = 'Counts', Lactobacillus.crispatus, 
         Lactobacillus.gasseri, Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, Eukaryote,
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)
bac3 <- bac2 %>% 
  select (Bacteria, Counts) %>% 
  group_by(Bacteria) %>%
  summarize(TotalCounts = sum(Counts)) %>%
  select (TotalCounts)
#specnumber


#cohort richness
rarefy2 <- rarefy(bac3, sample=min(rowSums(bac3))) #may be h
#write cohort species richness into file
rarefy2 <- as.data.frame(rarefy2)
#write.csv(rarefy2, "1Arichness_cohort.csv")b #will fix headings in excel


#Plot rarefraction curve
bac <- data %>%
  select(Participants, Lactobacillus.crispatus, Lactobacillus.gasseri, 
         Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, Eukaryote, 
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)
#want rownames to be participants
rownames(bac) <- bac[,1]
bac[,1] <- NULL
#colours for individuals
col <- c('deepskyblue3', 'cornflowerblue', 'deepskyblue', 'green3', 
         'forestgreen', 'palegreen', 'green', 'darkgoldenrod1', 
         'purple', 'mediumorchid2', 'plum', 'firebrick', 'yellow', 'firebrick1', 
         'gray33', 'gray', 'mediumvioletred', 'black', 'olivedrab2', 
         'orange3', 'tomato', 'lightsalmon', 'slateblue', 'turquoise', 
         'lavender', 'rosybrown2', 'deeppink')
rarecurve(bac, step = 27, sample = min(rowSums(bac)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Bacterial Species", 
          label = FALSE, col = col, xlim=c(0,17500), lwd = 2)
#each line is rarefied richness value (higher values more rich than lower values)
#xaxis:# of indvidual species present in each participant
#yaxis: bacterial species and amount each participant has
#need to figure out legend (do not think there is one)
#can alter x-axis to see some samples more distinctly 

#section earlier curves to see more indepth
rarecurve(bac, step = 27, sample = min(rowSums(bac)), 
          xlab = "Sequence Read Counts", ylab = "Number of Different Bacterial Species", 
          label = FALSE, col = col, xlim=c(0,8500), lwd = 2)

