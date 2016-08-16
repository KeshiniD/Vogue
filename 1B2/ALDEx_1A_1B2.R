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

#Nugent.score
cond.edu <- meta$Nugent.score
ald.edu5 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu5$glm.eBH) # 1.241176e-15
min(ald.edu5$kw.eBH) # 7.524657e-13

#Age
cond.edu <- meta$Age
ald.edu6 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu6$glm.eBH) #1.017653e-07
min(ald.edu6$kw.eBH) #0.8410674

#BMI
cond.edu <- meta$BMI
ald.edu7 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu7$glm.eBH) # 0.001839731
min(ald.edu7$kw.eBH) # 0.855607

#BV_2months
cond.edu <- meta$BV..number.of.episodes.2.months.
ald.edu8 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu8$glm.eBH) # 0.002328189
min(ald.edu8$kw.eBH) # 0.0337454

#BV_year
cond.edu <- meta$BV..number.of.episodes.year.
ald.edu9 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu9$glm.eBH) # 0.004781706
min(ald.edu9$kw.eBH) # 0.1478793

#BV_lifetime
cond.edu <- meta$BV..number.of.episodes.lifetime.
ald.edu10 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu10$glm.eBH) #0.007302841
min(ald.edu10$kw.eBH) #0.2387352

#Yeast_2months
cond.edu <- meta$Yeast..2months.
ald.edu11 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu11$glm.eBH) #0.7945676
min(ald.edu11$kw.eBH) # 0.8814478

#Yeast_year
cond.edu <- meta$Yeast..year.
ald.edu12 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu12$glm.eBH) #0.2254619
min(ald.edu12$kw.eBH) # 0.8938613

#Yeast_lifetime
cond.edu <- meta$Yeast..lifetime.
ald.edu13 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu13$glm.eBH) #0.3590241
min(ald.edu13$kw.eBH) # 0.9092517

#Antimicrobials (make into factor)
cond.edu <- meta$Antimicrobial.Use..y.1..n.0.
ald.edu14 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu14$glm.eBH) #0.2931287
min(ald.edu14$kw.eBH) #0.541405

#Prescription (make into factor)
cond.edu <- meta$X.Non..Prescription..y.1..n.0.
ald.edu15 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu15$glm.eBH) #0.8166254
min(ald.edu15$kw.eBH) # 0.8315676

#vaginal intercourse_past48h (make into factor)
cond.edu <- meta$Vaginal.intercourse.in.past.48.hours..y.1..n.0.
ald.edu16 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu16$glm.eBH) #0.3078194
min(ald.edu16$kw.eBH) # 0.5789654

#oralsex_cat (factor)
cond.edu <- meta$Freq.oral.sex.cat
ald.edu17 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu17$glm.eBH) # 0.8288082
min(ald.edu17$kw.eBH) # 0.8253596

#analsex_cat (factor)
cond.edu <- meta$Freq.anal.sex.cat
ald.edu18 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu18$glm.eBH) # 0.5200745
min(ald.edu18$kw.eBH) # 0.4972258

#sextoy_cat (factor)
cond.edu <- meta$Freq.sex.toy.use.cat
ald.edu19 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu19$glm.eBH) #0.7256718
min(ald.edu19$kw.eBH) #0.7233183

#presencesymptoms_2wks
cond.edu <- meta$Presence.Symptoms.2wks
ald.edu20 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu20$glm.eBH) 
min(ald.edu20$kw.eBH)

#abnormdischarge_2wks
cond.edu <- meta$Abnormal.discharge.2wks
ald.edu21 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu21$glm.eBH) 
min(ald.edu21$kw.eBH)

#abnormodor_2wks
cond.edu <- meta$Abnormal.odor.2wks
ald.edu22 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu22$glm.eBH) 
min(ald.edu22$kw.eBH)

#irritationdiscomfort_2wks
cond.edu <- meta$Irritation.Discomfort.2wks
ald.edu23 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu23$glm.eBH) 
min(ald.edu23$kw.eBH)

#other_2wks
cond.edu <- meta$Other.Symptoms.2wks
ald.edu24 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu24$glm.eBH) 
min(ald.edu24$kw.eBH)

#presencesymptoms_48h
cond.edu <- meta$Presence.Symptoms.48hrs
ald.edu25 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu25$glm.eBH) 
min(ald.edu25$kw.eBH)

#abnormdischarge_48h
cond.edu <- meta$Abnormal.discharge.48hrs
ald.edu26 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu26$glm.eBH) 
min(ald.edu26$kw.eBH)

#abnormodor_48h
cond.edu <- meta$Abnormal.odor.48hrs
ald.edu27 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu27$glm.eBH) 
min(ald.edu27$kw.eBH)

#irritationdiscomfort_48h
cond.edu <- meta$Irritation.Discomfort.48hrs
ald.edu28 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu28$glm.eBH) 
min(ald.edu28$kw.eBH)

#other_48h
cond.edu <- meta$Other.Symptoms.48hrs
ald.edu29 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu29$glm.eBH) 
min(ald.edu29$kw.eBH)

#symptompain
cond.edu <- meta$Symptom.pain
ald.edu30 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu30$glm.eBH) 
min(ald.edu30$kw.eBH)

#genwarts_ever
cond.edu <- meta$Genwarts.ever
ald.edu31 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu31$glm.eBH) 
min(ald.edu31$kw.eBH)

#chlamydia_ever
cond.edu <- meta$Chlamydia.ever
ald.edu32 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu32$glm.eBH) 
min(ald.edu32$kw.eBH)

#uti_ever
cond.edu <- meta$UTI.ever
ald.edu33 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu33$glm.eBH) 
min(ald.edu33$kw.eBH)

#trich_ever
cond.edu <- meta$Trich.ever
ald.edu34 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu34$glm.eBH) 
min(ald.edu34$kw.eBH)

#genitalherpes_ever
cond.edu <- meta$GenHerpes.ever
ald.edu35 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu35$glm.eBH) 
min(ald.edu35$kw.eBH)

#numberpartners_year
cond.edu <- meta$Number.partners.in.past.year.cat
ald.edu36 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu36$glm.eBH) 
min(ald.edu36$kw.eBH)

#contraception_H
cond.edu <- meta$contraception.H
ald.edu37 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu37$glm.eBH) 
min(ald.edu37$kw.eBH)

#contraception_BM
cond.edu <- meta$contraception.B.M
ald.edu38 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu38$glm.eBH) 
min(ald.edu38$kw.eBH)

#contraception_IUD
cond.edu <- meta$contraception.C.IUD
ald.edu39 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu39$glm.eBH) 
min(ald.edu39$kw.eBH)

#contraception_none
cond.edu <- meta$Contraception.none
ald.edu40 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu40$glm.eBH) 
min(ald.edu40$kw.eBH)

#substanceuse
cond.edu <- meta$Substance.Use
ald.edu41 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu41$glm.eBH) 
min(ald.edu41$kw.eBH)

#studyarm
cond.edu <- meta$study_arm
ald.edu42 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu42$glm.eBH) 
min(ald.edu42$kw.eBH)

#Tamponuse_cat
cond.edu <- meta$Tampon.Use.cat
ald.edu43 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu43$glm.eBH) 
min(ald.edu43$kw.eBH)

#preg_cat
cond.edu <- meta$Pregnancy.cat
ald.edu44 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu44$glm.eBH) 
min(ald.edu44$kw.eBH)

#smokingcurrent
cond.edu <- meta$smoking.current
ald.edu45 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu45$glm.eBH) 
min(ald.edu45$kw.eBH)

#feminineproducts_48h
cond.edu <- meta$Feminine.products.48hrs
ald.edu46 <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
# smallest p-values
min(ald.edu46$glm.eBH) 
min(ald.edu46$kw.eBH)

#####################################
#Dean put all variables in one line
library(ALDEx2)
meta <- read.csv(file="Aldex_metadata_1A_1B2.csv")
bac <- read.csv(file="Aldex_bac_1A_1B2")

#variables need to be factors; maybe? try integer and see
# set the rownames as the taxa names
row.names(bac) <- bac[, 1]
bac <- bac[, -1]
meta$X <- NULL



variables <- colnames(meta)
#variables <- variables[c(2,7,17)]
notfactors <- c(
  "Nugent.score","Age","BMI","BV..number.of.episodes.2.months.",
  "BV..number.of.episodes.year.","BV..number.of.episodes.lifetime.",
  "Yeast..2months.","Yeast..year.","Yeast..lifetime."
)


mydf <- data.frame(variable = c(), glm.eBH = c(), kw.eBH = c())

lapply(variables, function(var) {
  
  if ( var == "Participants" ) {
    return()
  }
  if (!(var %in% notfactors)) {
    var < as.factor(var)
  }
  
  #make a vector that is the variable labels
  cond.edu <- meta[[var]]
  
  #run ALDEx
  ald.edu <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE)
  #ald.edu <- aldex(reads = bac, conditions = cond.edu, test = "glm", effect = FALSE, mc.samples = 2, verbose = FALSE)
  
  #look at the output
  #head(ald.edu)
  cat(var, "\t", min(ald.edu$glm.eBH), "\t", min(ald.edu$kw.eBH), "\n")
  row <- data.frame(variable = var, glm.eBH = min(ald.edu$glm.eBH), kw.eBH = min(ald.edu$kw.eBH))
  mydf <<- rbind(mydf, row)
})

mydf$signif <- mydf$glm.eBH < 0.05

#################
mydf2 <- mydf
mydf2$signif <- mydf$glm.eBH < 0.05
View(mydf2)
mydf2$signif <- mydf$glm.eBH < 0.05

#################################################################################
#look at variables which are significant
meta$X.Non..Prescription..y.1..n.0. <- factor(meta$X.Non..Prescription..y.1..n.0.)
meta$Vaginal.intercourse.in.past.48.hours..y.1..n.0. <- factor(meta$Vaginal.intercourse.in.past.48.hours..y.1..n.0.)
meta$Abnormal.odor.2wks <- factor(meta$Abnormal.odor.2wks)
meta$Other.Symptoms.2wks <- factor(meta$Other.Symptoms.2wks)
meta$Abnormal.odor.48hrs <- factor(meta$Abnormal.odor.48hrs)
meta$Other.Symptoms.48hrs <- factor(meta$Other.Symptoms.48hrs)
meta$Genwarts.ever <- factor(meta$Genwarts.ever)
meta$Substance.Use <- factor(meta$Substance.Use)
meta$study_arm <- factor(meta$study_arm)
meta$Tampon.Use.cat <- factor(meta$Tampon.Use.cat)
meta$Chlamydia.ever <- factor(meta$Chlamydia.ever)
meta$Trich.ever <- factor(meta$Trich.ever)
meta$GenHerpes.ever <- factor(meta$GenHerpes.ever)
meta$Pregnancy.cat <- factor(meta$Pregnancy.cat)
meta$CST <- factor(meta$CST)

#Nugent Score
cond.eduNS <- meta$Nugent.score
ald.eduNS <- aldex(reads = bac, conditions = cond.eduNS, test = "glm", effect = FALSE)

#Age 
cond.eduAge <- meta$Age
ald.eduAge <- aldex(reads = bac, conditions = cond.eduAge, test = "glm", effect = FALSE)

#BMI 
cond.eduBMI <- meta$BMI
ald.eduBMI <- aldex(reads = bac, conditions = cond.eduBMI, test = "glm", effect = FALSE)

#BV..number.of.episodes.2.months.
cond.eduBV2mths <- meta$BV..number.of.episodes.2.months.
ald.eduBV2mths <- aldex(reads = bac, conditions = cond.eduBV2mths, test = "glm", effect = FALSE)

#BV..number.of.episodes.year.
cond.eduBVyr <- meta$BV..number.of.episodes.year.
ald.eduBVyr <- aldex(reads = bac, conditions = cond.eduBVyr, test = "glm", effect = FALSE)

#BV..number.of.episodes.lifetime.
cond.eduBVlifetime <- meta$BV..number.of.episodes.lifetime.
ald.eduBVlifetime <- aldex(reads = bac, conditions = cond.eduBVlifetime, test = "glm", effect = FALSE)

#X.Non..Prescription..y.1..n.0.
cond.edurxdrug <- meta$X.Non..Prescription..y.1..n.0.
ald.edurxdrug <- aldex(reads = bac, conditions = cond.edurxdrug, test = "glm", effect = FALSE)

#Vaginal.intercourse.in.past.48.hours..y.1..n.0.
cond.eduintercourse48 <- meta$Vaginal.intercourse.in.past.48.hours..y.1..n.0.
ald.eduintercourse48 <- aldex(reads = bac, conditions = cond.eduintercourse48, test = "glm", effect = FALSE)

#Abnormal.odor.2wks 
cond.eduodor2wks <- meta$Abnormal.odor.2wks
ald.eduodor2wks <- aldex(reads = bac, conditions = cond.eduodor2wks, test = "glm", effect = FALSE)

#Other.Symptoms.2wks
cond.eduother2wks <- meta$Other.Symptoms.2wks
ald.eduother2wks <- aldex(reads = bac, conditions = cond.eduother2wks, test = "glm", effect = FALSE)

#Abnormal.odor.48hrs
cond.eduodor48hrs <- meta$Abnormal.odor.48hrs
ald.eduodor48hrs <- aldex(reads = bac, conditions = cond.eduodor48hrs, test = "glm", effect = FALSE)

#Other.Symptoms.48hrs
cond.eduother48hrs <- meta$Other.Symptoms.48hrs
ald.eduother48hrs <- aldex(reads = bac, conditions = cond.eduother48hrs, test = "glm", effect = FALSE)

#Genwarts.ever
cond.edugenwarts <- meta$Genwarts.ever
ald.edugenwarts <- aldex(reads = bac, conditions = cond.edugenwarts, test = "glm", effect = FALSE)

#Substance.Use
cond.edusubuse <- meta$Substance.Use
ald.edusubuse <- aldex(reads = bac, conditions = cond.edusubuse, test = "glm", effect = FALSE)

#study_arm
cond.edustudyarm <- meta$study_arm
ald.edustudyarm <- aldex(reads = bac, conditions = cond.edustudyarm, test = "glm", effect = FALSE)

#Tampon.Use.cat
cond.edutamponuse <- meta$Tampon.Use.cat
ald.edutamponuse <- aldex(reads = bac, conditions = cond.edutamponuse, test = "glm", effect = FALSE)

#Chlamydia.ever
cond.educhlamydia <- meta$Chlamydia.ever
ald.educhlamydia <- aldex(reads = bac, conditions = cond.educhlamydia, test = "glm", effect = FALSE)

#Trich.ever
cond.edutrich <- meta$Trich.ever
ald.edutrich <- aldex(reads = bac, conditions = cond.edutrich, test = "glm", effect = FALSE)

#GenHerpes.ever
cond.edugenherpes <- meta$GenHerpes.ever
ald.edugenherpes <- aldex(reads = bac, conditions = cond.edugenherpes, test = "glm", effect = FALSE)

#Pregnancy.cat
cond.edupreg <- meta$Pregnancy.cat
ald.edupreg <- aldex(reads = bac, conditions = cond.edupreg, test = "glm", effect = FALSE)

#CST
cond.eduCST <- meta$CST
ald.eduCST <- aldex(reads = bac, conditions = cond.eduCST, test = "glm", effect = FALSE)

###############
#write to file
#write.csv(ald.eduNS, "Aldex_1B2_Nugentscore.csv")
#write.csv(ald.eduAge, "Aldex_1B2_Age.csv")
#write.csv(ald.eduBMI, "Aldex_1B2_BMI.csv")
#write.csv(ald.eduBV2mths, "Aldex_1B2_BV2months.csv")
#write.csv(ald.eduBVyr, "Aldex_1B2_BVyear.csv")
#write.csv(ald.eduBVlifetime, "Aldex_1B2_BVlifetime.csv")
#write.csv(ald.edurxdrug, "Aldex_1B2_rxdrug.csv")
#write.csv(ald.eduintercourse48, "Aldex_1B2_intercourse48hrs.csv")
#write.csv(ald.eduodor2wks, "Aldex_1B2_odor2wks.csv")
#write.csv(ald.eduother2wks, "Aldex_1B2_other2wks.csv")
#write.csv(ald.eduodor48hrs, "Aldex_1B2_odor48hrs.csv")
#write.csv(ald.eduother48hrs, "Aldex_1B2_other48hrs.csv")
#write.csv(ald.edugenwarts, "Aldex_1B2_Genwarts.csv")
#write.csv(ald.edusubuse, "Aldex_1B2_substanceuse.csv")
#write.csv(ald.edustudyarm, "Aldex_1B2_studyarm.csv")
#write.csv(ald.edutamponuse, "Aldex_1B2_TamponUse.csv")
#write.csv(ald.educhlamydia, "Aldex_1B2_Chlamydia.csv")
#write.csv(ald.edutrich, "Aldex_1B2_Trich.csv")
#write.csv(ald.edugenherpes, "Aldex_1B2_GenHerpes.csv")
#write.csv(ald.edupreg, "Aldex_1B2_Pregcat.csv")
#write.csv(ald.eduCST, "Aldex_1B2_CST.csv")
#################################################################################
#aug-16-16
#add yeast.ever; not significant 
meta$Yeast.ever <- ifelse(meta$Yeast..lifetime. > 0, 
                           c("1"), c("0")) 
#convert Yeast.ever from character into factor
meta$Yeast.ever <- factor(meta$Yeast.ever) 

#CST
cond.eduYeast <- meta$Yeast.ever
ald.eduYeast <- aldex(reads = bac, conditions = cond.eduYeast, test = "glm", effect = FALSE)


#aug-16-16
#median values to define significant assoications from aldex
#Dean put all variables in one line
library(ALDEx2)
meta <- read.csv(file="Aldex_metadata_1A_1B2.csv")
bac <- read.csv(file="Aldex_bac_1A_1B2")

# set the rownames as the taxa names
row.names(bac) <- bac[, 1]
bac <- bac[, -1]
meta$X <- NULL

#only handles tow levels; not more than one
x <- aldex.clr(bac, mc.samples=128)
a <- aldex.effect(x, conditions = meta$Freq.oral.sex.cat, include.sample.summary = TRUE)
