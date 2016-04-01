source("https://bioconductor.org/biocLite.R")
biocLite("ALDEx2")
library(ALDEx2)
library(plyr)
library(dplyr)
library(tidyr)

#called recent data participants 26 to 64
nwdata <- read.delim(file.path("1B2_26_64.txt"))

#called older data participants 01 to 29 (26 in newer data)
olddata <- read.delim(file.path ("1B2data_part1.txt"))

#1A participants, all 310
data1A <- read.csv(file="1A_full_bac.csv")

#selected all 1A participants, and simples names column
#data1A <- read.csv(file="1A_full_bac.csv")
dean <- read.csv(file.path("1A.csv"))
nums <- substring(dean$Study.ID, 12)
ids <- paste0("Vogue1A.01.", nums)
data1A <- data1A[, which(colnames(data1A) %in% c(ids, "Simple.names"))]

#removed unnecessary columns
nwdata$Group <- NULL
nwdata$TOTAL.frequency <- NULL

#removed unnecessary columns
olddata$Group <- NULL
olddata$Total.Frequency <- NULL
olddata$Vogue1B2.01.05 <- NULL
olddata$Vogue1B2.01.04 <- NULL
olddata$Vogue1B2.01.20 <- NULL
olddata$Vogue1B2.01.14 <- NULL
olddata$Vogue1B2.01.16 <- NULL
olddata$Vogue1B2.01.03 <- NULL
olddata$Vogue1B2.01.24 <- NULL
olddata$Vogue1B2.01.13 <- NULL

#rename column 
data1A <- dplyr::rename(data1A, Simple.name = Simple.names)

#collapse similar bacterial species together
nwdata2 <- ddply(nwdata,c("Simple.name"),numcolwise(sum))

#merge datasets
zz <-join(nwdata2, olddata, type="full")
data1AB2 <-join(zz, data1A, type="full")
data1AB2[is.na(data1AB2)] <- 0 # makes NAs into 0s

#write to file
#write.csv(data1AB2, "full_1A_1B2.csv")
#fixed inconsistent names

#call back
data <- read.csv(file="full_1A_1B2.csv")

#collapse same bacterial species into one, and remove first column
data$X <- NULL
data2 <- ddply(data,c("Simple.name"),numcolwise(sum))

# set the rownames as the taxa names
row.names(data2) <- data2[, 1]
data2 <- data2[, -1]

#rename vogue 1B2 participants and order based on participants
data3 <- dplyr::rename(data2, Vogue1B2.01.26 = X1B2_01_26, 
                      Vogue1B2.01.35 = X1B2_01_35, Vogue1B2.01.37 = X1B2_01_37, 
                      Vogue1B2.01.38 = X1B2_01_38, Vogue1B2.01.50 = X1B2_01_50, 
                      Vogue1B2.01.52 = X1B2_01_52, Vogue1B2.01.56 = X1B2_01_56, 
                      Vogue1B2.01.58 = X1B2_01_58, Vogue1B2.01.61 = X1B2_01_61, 
                      Vogue1B2.01.62 = X1B2_01_62, Vogue1B2.01.63 = X1B2_01_63, 
                      Vogue1B2.01.64 = X1B2_01_64)

data4 <- data3[, order(names(data3))]

#do same to metadata variables
total <- read.csv(file = "1A_1B2_compare.csv")

#order alphatbetically
total2 <- total[order(total$Participants),]
#removed extra column
total2$X <- NULL

#########################
# ALDEx can be run on any number of categories of your metatdata

#BV
table(total2$CST)
#I  II III IVA IVC IVD   V 
#161   1  56  42  28  26  22 

# make a vector that is the variable labels
cond.edu <- total2$CST
# run ALDEx
ald.edu <- aldex(reads = data4, conditions = cond.edu, test = "glm", effect = FALSE)

# look at the output
head(ald.edu)

# smallest p-values
min(ald.edu$glm.eBH) # 2.7962e-81
min(ald.edu$kw.eBH) # 9.77707e-46

#write metadata and bacterial into file to look at later
#write.csv(total2, "Aldex_metadata_1A_1B2.csv")
#write.csv(data4, "Aldex_bac_1A_1B2")

########################################################################
#Mar-5-16
#cleaning and testing done above
meta <- read.csv(file="Aldex_metadata_1A_1B2.csv")
bac <- read.csv(file="Aldex_bac_1A_1B2")

#variables need to be factors; maybe? try integer and see
# set the rownames as the taxa names
row.names(bac) <- bac[, 1]
bac <- bac[, -1]

#make a vector that is the variable labels
cond.edu <- meta$CST
#run ALDEx
ald.edu <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)

#look at the output
head(ald.edu)

# smallest p-values
min(ald.edu$glm.eBH) # 8.105249e-85
min(ald.edu$kw.eBH) # 9.634603e-46

########################
#variables need to be factors; maybe? try integer and see
#Feminine Products
cond.edu <- meta$Feminine.products
ald.edu1 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu1$glm.eBH) # 0.9318553
min(ald.edu1$kw.eBH) # 0.9345083

#Condoms.past.48hrs
cond.edu <- meta$condoms.48h
ald.edu2 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu2$glm.eBH) # 0.540174
min(ald.edu2$kw.eBH) # 0.6268818

#TamponUse.past.month
cond.edu <- meta$Tampon.use.1mth
ald.edu3 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu3$glm.eBH) # 0.8178718
min(ald.edu3$kw.eBH) #  0.7903009

#Ethnicity
cond.edu <- meta$Ethnicity.cat
ald.edu4 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu4$glm.eBH) #0.6617548
min(ald.edu4$kw.eBH) # 0.6399051

#Ethnicity
cond.edu <- meta$Nugent.score
ald.edu5 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu5$glm.eBH) # 1.241176e-15
min(ald.edu5$kw.eBH) # 7.524657e-13