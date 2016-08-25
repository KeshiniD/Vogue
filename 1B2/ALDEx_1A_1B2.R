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

####################################
#####################################
#Dean put all variables in one line
library(ALDEx2)
meta <- read.csv(file="Aldex_metadata_1A_1B2_v2.csv") #edits made below
bac <- read.csv(file="Aldex_bac_1A_1B2")

#variables need to be factors; maybe? try integer and see
# set the rownames as the taxa names
row.names(bac) <- bac[, 1]
bac <- bac[, -1]
meta$X <- NULL
meta$X.4 <- NULL
meta$X.3 <- NULL
meta$X.2 <- NULL
meta$X.1 <- NULL


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

#Yeast
cond.eduYeast <- meta$Yeast.ever
ald.eduYeast <- aldex(reads = bac, conditions = cond.eduYeast, test = "glm", effect = FALSE)


#aug-16-16
#median values to define significant assoications from aldex
library(ALDEx2)
meta <- read.csv(file="Aldex_metadata_1A_1B2_v2.csv")
bac <- read.csv(file="Aldex_bac_1A_1B2")

# set the rownames as the taxa names
row.names(bac) <- bac[, 1]
bac <- bac[, -1]
meta$X <- NULL

#only handles two levels; not more than one
clr <- aldex.clr(bac, mc.samples=128)
# a <- aldex.effect(x, conditions = meta$Freq.oral.sex.cat, include.sample.summary = TRUE)

#put significant variables into 2 levels if not already
#BV2mths
#grouping BV.2months
meta$BV.2mths.cat <- ifelse(meta$BV..number.of.episodes.2.months. > 0, 
                              c("1"), c("0")) 
#convert BV.2mths from character into factor
meta$BV.2mths.cat <- factor(meta$BV.2mths.cat) 

bV.2mths <- aldex.effect(clr, conditions = meta$BV.2mths.cat, include.sample.summary = TRUE)

#cst
meta$CST.cat[meta$CST=='I'] <- '0'
meta$CST.cat[meta$CST=='II'] <- '0'
meta$CST.cat[meta$CST=='III'] <- '1'
meta$CST.cat[meta$CST=='IVA'] <- '1'
meta$CST.cat[meta$CST=='IVC'] <- '1'
meta$CST.cat[meta$CST=='IVD'] <- '1'
meta$CST.cat[meta$CST=='V'] <- '0'

#convert UTI.ever from character into factor
meta$CST.cat <- factor(meta$CST.cat)

CST <- aldex.effect(clr, conditions = meta$CST.cat, include.sample.summary = TRUE)

#nug.score
meta$Nugent.score.cat <- ifelse(meta$Nugent.score > 3, 
                               c("1"), c("0")) 
#convert UTI.ever from character into factor
meta$Nugent.score.cat <- factor(meta$Nugent.score.cat) 

nugent <- aldex.effect(clr, conditions = meta$Nugent.score.cat, include.sample.summary = TRUE)

#study_arm
study_arm <- aldex.effect(clr, conditions = meta$study_arm, include.sample.summary = TRUE)

#write median values to file
# write.csv(bV.2mths, "Aldex_median_bv2mths.csv")
# write.csv(CST, "Aldex_median_cst.csv")
# write.csv(nugent, "Aldex_median_nugent.csv")
# write.csv(study_arm, "Aldex_median_studyarm.csv")

##################################################################
#Aug-18-16
#noticed coding between 1A and 1B2 did not align, some variables have more than 2 levels (0,1,2); fix
#split 1A and 1B2, align 1A coding with 1B2 and merge together

metaA <- meta[which(meta$study_arm=="1A"),]
metaB2 <- meta[which(meta$study_arm=="1B2"),]

#antimicro
summary(factor(metaA$Antimicrobial.Use..y.1..n.0.))
summary(factor(metaB2$Antimicrobial.Use..y.1..n.0.))

#convert yes-2, no-1 into yes-1, no-0
metaA$Antimicrobial.Use..y.1..n.0. <- ifelse(metaA$Antimicrobial.Use..y.1..n.0. > 1, 
                         c("1"), c("0")) 
#convert antimicro from character into factor
metaA$Antimicrobial.Use..y.1..n.0. <- factor(metaA$Antimicrobial.Use..y.1..n.0.) 

###
#rxdrug
summary(factor(metaA$X.Non..Prescription..y.1..n.0.))
summary(factor(metaB2$X.Non..Prescription..y.1..n.0.))

##convert yes-2, no-1 into yes-1, no-0
metaA$X.Non..Prescription..y.1..n.0. <- ifelse(metaA$X.Non..Prescription..y.1..n.0. > 1, 
                                             c("1"), c("0")) 
#convert rxdrug from character into factor
metaA$X.Non..Prescription..y.1..n.0. <- factor(metaA$X.Non..Prescription..y.1..n.0.) 

###
#vgint.48h
summary(factor(metaA$Vaginal.intercourse.in.past.48.hours..y.1..n.0.))
summary(factor(metaB2$Vaginal.intercourse.in.past.48.hours..y.1..n.0.))

##convert yes-2, no-1 into yes-1, no-0
metaA$Vaginal.intercourse.in.past.48.hours..y.1..n.0. <- ifelse(metaA$Vaginal.intercourse.in.past.48.hours..y.1..n.0. > 1, 
                                               c("1"), c("0")) 
#convert from character into factor
metaA$Vaginal.intercourse.in.past.48.hours..y.1..n.0. <- factor(metaA$Vaginal.intercourse.in.past.48.hours..y.1..n.0.) 

##
#abnormdischarge.2wks
summary(factor(metaA$Abnormal.discharge.2wks))
summary(factor(metaB2$Abnormal.discharge.2wks))

##convert yes-2, no-1 into yes-1, no-0
metaA$Abnormal.discharge.2wks <- ifelse(metaA$Abnormal.discharge.2wks > 1, 
                                                                c("1"), c("0")) 
#convert from character into factor
metaA$Abnormal.discharge.2wks <- factor(metaA$Abnormal.discharge.2wks) 

###
#abnormdischarge.48h
summary(factor(metaA$Abnormal.discharge.48hrs))
summary(factor(metaB2$Abnormal.discharge.48hrs))

##convert yes-2, no-1 into yes-1, no-0
metaA$Abnormal.discharge.48hrs <- ifelse(metaA$Abnormal.discharge.48hrs > 1, 
                                        c("1"), c("0")) 
#convert from character into factor
metaA$Abnormal.discharge.48hrs <- factor(metaA$Abnormal.discharge.48hrs) 

###
#abnormodor.2wks
summary(factor(metaA$Abnormal.odor.2wks))
summary(factor(metaB2$Abnormal.odor.2wks))

##convert yes-2, no-1 into yes-1, no-0
metaA$Abnormal.odor.2wks <- ifelse(metaA$Abnormal.odor.2wks > 1, 
                                         c("1"), c("0")) 
#convert from character into factor
metaA$Abnormal.odor.2wks <- factor(metaA$Abnormal.odor.2wks) 

#####
#abnormodor.48h
summary(factor(metaA$Abnormal.odor.48hrs))
summary(factor(metaB2$Abnormal.odor.48hrs))

##convert yes-2, no-1 into yes-1, no-0
metaA$Abnormal.odor.48hrs <- ifelse(metaA$Abnormal.odor.48hrs > 1, 
                                   c("1"), c("0")) 
#convert from character into factor
metaA$Abnormal.odor.48hrs <- factor(metaA$Abnormal.odor.48hrs) 

###
#irritation/discomfor.2weeks
summary(factor(metaA$Irritation.Discomfort.2wks))
summary(factor(metaB2$Irritation.Discomfort.2wks))

##convert yes-2, no-1 into yes-1, no-0
metaA$Irritation.Discomfort.2wks <- ifelse(metaA$Irritation.Discomfort.2wks > 1, 
                                   c("1"), c("0")) 
#convert from character into factor
metaA$Irritation.Discomfort.2wks <- factor(metaA$Irritation.Discomfort.2wks) 

###
#irritation/discomfort.48h
summary(factor(metaA$Irritation.Discomfort.48hrs))
summary(factor(metaB2$Irritation.Discomfort.48hrs))

##convert yes-2, no-1 into yes-1, no-0
metaA$Irritation.Discomfort.48hrs <- ifelse(metaA$Irritation.Discomfort.48hrs > 1, 
                                   c("1"), c("0")) 
#convert from character into factor
metaA$Irritation.Discomfort.48hrs <- factor(metaA$Irritation.Discomfort.48hrs) 

###
#othervag.2weeks
summary(factor(metaA$Other.Symptoms.2wks))
summary(factor(metaB2$Other.Symptoms.2wks))

##convert yes-2, no-1 into yes-1, no-0
metaA$Other.Symptoms.2wks <- ifelse(metaA$Other.Symptoms.2wks > 1, 
                                   c("1"), c("0")) 
#convert from character into factor
metaA$Other.Symptoms.2wks <- factor(metaA$Other.Symptoms.2wks) 

###
#other.symptoms.48h
summary(factor(metaA$Other.Symptoms.48hrs))
summary(factor(metaB2$Other.Symptoms.48hrs))

##convert yes-2, no-1 into yes-1, no-0
metaA$Other.Symptoms.48hrs <- ifelse(metaA$Other.Symptoms.48hrs > 1, 
                                   c("1"), c("0")) 
#convert from character into factor
metaA$Other.Symptoms.48hrs <- factor(metaA$Other.Symptoms.48hrs) 

###
#genwarts
summary(factor(metaA$Genwarts.ever))
summary(factor(metaB2$Genwarts.ever))

##convert yes-1, no-2 into yes-1, no-0
metaA$Genwarts.ever <- ifelse(metaA$Genwarts.ever > 1, 
                                   c("0"), c("1")) 
#convert from character into factor
metaA$Genwarts.ever <- factor(metaA$Genwarts.ever) 

###
#tampon.cat
summary(factor(metaA$Tampon.Use.cat))
summary(factor(metaB2$Tampon.Use.cat))

##convert yes-2, no-1 into yes-1, no-0
metaB2$Tampon.Use.cat <- ifelse(metaB2$Tampon.Use.cat > 1, 
                                   c("1"), c("0")) 
#convert from character into factor
metaB2$Tampon.Use.cat <- factor(metaB2$Tampon.Use.cat) 
metaA$Tampon.Use.cat <- factor(metaA$Tampon.Use.cat)

###
#chlamydia
summary(factor(metaA$Chlamydia.ever))
summary(factor(metaB2$Chlamydia.ever))

##convert yes-1, no-2 into yes-1, no-0
metaA$Chlamydia.ever[metaA$Chlamydia.ever >= 2 & metaA$Chlamydia.ever <=2] <- "0" #normal weight

#convert from character into factor
metaA$Chlamydia.ever <- factor(metaA$Chlamydia.ever) 

###
#uti
summary(factor(metaA$UTI.ever))
summary(factor(metaB2$UTI.ever))

##convert yes-1, no-2 into yes-1, no-0
metaA$UTI.ever[metaA$UTI.ever >= 2 & metaA$UTI.ever <=2] <- "0" #normal weight

#convert from character into factor
metaA$UTI.ever <- factor(metaA$UTI.ever) 

#trich
summary(factor(metaA$Trich.ever))
summary(factor(metaB2$Trich.ever))

##convert yes-1, no-2 into yes-1, no-0
metaA$Trich.ever[metaA$Trich.ever >= 2 & metaA$Trich.ever <=2] <- "0" #normal weight

#convert from character into factor
metaA$Trich.ever <- factor(metaA$Trich.ever) 

#herpes
summary(factor(metaA$GenHerpes.ever))
summary(factor(metaB2$GenHerpes.ever))

##convert yes-1, no-2 into yes-1, no-0
metaA$GenHerpes.ever[metaA$GenHerpes.ever >= 2 & metaA$GenHerpes.ever <=2] <- "0" #normal weight

#convert from character into factor
metaA$GenHerpes.ever <- factor(metaA$GenHerpes.ever)

###
#preg.cat
summary(factor(metaA$Pregnancy.cat))
summary(factor(metaB2$Pregnancy.cat))

##convert yes-2, no-1 into yes-1, no-0
metaA$Pregnancy.cat <- ifelse(metaA$Pregnancy.cat > 1, 
                                c("1"), c("0")) 
#convert from character into factor
metaA$Pregnancy.cat <- factor(metaA$Pregnancy.cat) 

###
#sexual.partners.past.year
summary(factor(metaA$Number.partners.in.past.year.cat))
summary(factor(metaB2$Number.partners.in.past.year.cat))

##0-0 1+-1
metaA$Number.partners.in.past.year.cat <- ifelse(metaA$Number.partners.in.past.year.cat > 0, 
                              c("1"), c("0")) 
#convert from character into factor
metaA$Number.partners.in.past.year.cat <- factor(metaA$Number.partners.in.past.year.cat) 

#mege 1A and 1B2
metaAB <- join(metaA, metaB2, type="full")

#write to file
# write.csv(metaAB, "Aldex_metadata_1A_1B2_v2.csv")

#added more categories; gonn and syph
total <- read.csv("Aldex_metadata_1A_1B2_v2.csv")
b <- read.csv(file="1A_full_grouped.csv")

#BV ever and Yeast ever.cat
b$BV.ever <- ifelse(b$bv_infect < 2, 
                        c("1"), c("0"))
b$BV.ever <- factor(b$BV.ever)

b$Yeast.ever <- ifelse(b$yeast_infect < 2, 
                           c("1"), c("0"))
b$Yeast.ever <- factor(b$Yeast.ever)

b$Gonorrhea.ever <- ifelse(b$gonor_infect < 2, 
                       c("1"), c("0"))
b$Gonorrhea.ever <- factor(b$Gonorrhea.ever)

b$Syphillis.ever <- ifelse(b$syph_infect < 2, 
                           c("1"), c("0"))
b$Syphillis.ever <- factor(b$Syphillis.ever)

#grab just bv and yeast ever etc, and put into aldex (manually insert 1b2 later)
b <- b %>% select(study_id, BV.ever, Yeast.ever, Gonorrhea.ever, Syphillis.ever)
b <- dplyr::rename(b, Participants = study_id)

total <- join(total, b, type="full")

# write.csv(total, "Aldex_metadata_1A_1B2_v2.csv")

#add in oral, anal, and sextoy (were cat wrong in 1A)
b <- read.csv(file="1A_full_grouped.csv")
total <- read.csv("Aldex_metadata_1A_1B2_v2.csv")

b$oralsxfrequency.cat <- ifelse(b$oralsxfrequency > 1, 
                           c("1"), c("0"))
b$oralsxfrequency.cat <- factor(b$oralsxfrequency.cat)

b$analsxfrequency.cat <- ifelse(b$analsxfrequency > 1, 
                                c("1"), c("0"))
b$analsxfrequency.cat <- factor(b$analsxfrequency.cat)

b$sextoyfrequency.cat <- ifelse(b$sextoyfrequency > 1, 
                                c("1"), c("0"))
b$sextoyfrequency.cat <- factor(b$sextoyfrequency.cat)

#grab just sex activity cat, and put into aldex
b <- b %>% select(study_id, sextoyfrequency.cat, oralsxfrequency.cat, analsxfrequency.cat)
b <- dplyr::rename(b, Participants = study_id, Freq.oral.sex.cat = oralsxfrequency.cat, 
                   Freq.anal.sex.cat = analsxfrequency.cat, 
                   Freq.sex.toy.use.cat = sextoyfrequency.cat)

a <- read.csv(file="1B2metabac_condensedv2.csv")
a2 <- a %>% select(Participants, Freq.oral.sex.cat, Freq.anal.sex.cat, Freq.sex.toy.use.cat)

#make sure cats in total and b are factors before merging
both <- join(a2, b, type="full")

#I took sexual activity cats from 1B2, and merged with 1A, and now will merge this new data frame
#with the aldex code, after removing the existing columns
total <- join(total, both, type="full")

# write.csv(total, "Aldex_metadata_1A_1B2_v2.csv") #manually fixed smoking.current

#redo Aldex for those variables I altered
#Dean put all variables in one line
library(ALDEx2)
meta <- read.csv(file="Aldex_metadata_1A_1B2_v2.csv") #edits made below
bac <- read.csv(file="Aldex_bac_1A_1B2")

#variables need to be factors; maybe? try integer and see
# set the rownames as the taxa names
row.names(bac) <- bac[, 1]
bac <- bac[, -1]
meta$X <- NULL
meta$X.4 <- NULL
meta$X.3 <- NULL
meta$X.2 <- NULL
meta$X.1 <- NULL

meta <- meta %>%
  select(Participants, Antimicrobial.Use..y.1..n.0., X.Non..Prescription..y.1..n.0., 
         Vaginal.intercourse.in.past.48.hours..y.1..n.0., Abnormal.discharge.2wks, 
         Abnormal.discharge.48hrs, Abnormal.odor.2wks, Abnormal.odor.48hrs, 
         Irritation.Discomfort.2wks, Irritation.Discomfort.48hrs, Other.Symptoms.2wks, 
         Other.Symptoms.48hrs, Number.partners.in.past.year.cat, Tampon.Use.cat, Pregnancy.cat,
         Genwarts.ever, GenHerpes.ever, smoking.current, Freq.oral.sex.cat, Freq.anal.sex.cat, 
         Freq.sex.toy.use.cat, Chlamydia.ever, Nugent.score)

variables <- colnames(meta)
#variables <- variables[c(2,7,17)]
notfactors <- c("Nugent.score")


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

mydf$signif <- mydf$kw.eBH < 0.05
########################################
#BV, Yeast, UTI, Trich, Gonorrhea, Syphillis, Substanceuse
#Gonorrhea; not significant
cond.eduGon <- meta$Gonorrhea.ever
ald.eduGon <- aldex(reads = bac, conditions = cond.eduGon, test = "glm", effect = FALSE)

#Syphillis; not significant
cond.eduSyph <- meta$Syphillis.ever
ald.eduSyph <- aldex(reads = bac, conditions = cond.eduSyph, test = "glm", effect = FALSE)

#add BV and others
b <- read.csv(file="1A_full_grouped.csv")

#BV ever and Yeast ever.cat etc (BV and Yeast taken from above)
b$UTI.ever <- ifelse(b$uti_infect < 2, 
                     c("1"), c("0"))
b$UTI.ever <- factor(b$UTI.ever)

b$Trich.ever <- ifelse(b$trich_infect < 2, 
                       c("1"), c("0"))
b$Trich.ever <- factor(b$Trich.ever)

#grab just genital infection hx, and put into aldex
b <- b %>% select(study_id, UTI.ever, Trich.ever, BV.ever, Yeast.ever)
b <- dplyr::rename(b, Participants = study_id)

a <- read.csv(file="Vogue1B2_medhx.csv")
a2 <- a %>% select(Participants, BV.ever, Yeast.ever, UTI.ever, Trich.ever)

#make sure cats in total and b are factors before merging
both <- join(a2, b, type="full")

#I took genital infection hx from 1B2, and merged with 1A, and now will merge this new data frame
#with the aldex code, after removing the existing columns
total <- join(total, both, type="full")

# write.csv(total, "Aldex_metadata_1A_1B2_v2.csv") #manually fixed smoking.current
# now look in aldex
meta <- read.csv("Aldex_metadata_1A_1B2_v2.csv")
meta <- meta %>%
  select(Participants, Nugent.score, BV.ever, Yeast.ever, UTI.ever, Trich.ever)

variables <- colnames(meta)
#variables <- variables[c(2,7,17)]
notfactors <- c("Nugent.score")


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

mydf$signif <- mydf$kw.eBH < 0.05
# write.csv(mydf, "Aldex_1A_1B2_results_v3.csv")
#those that signficiant look at 
#none

##adding substance use
sub <- read.csv("Vogue_substanceuse_1A.csv")
b2 <- read.csv("1B2metabac_condensedv2.csv")

#subset 1b2 substance info
b2_subset <- b2 %>% select(Participants, Substance.Use, smoking.current)

#regroup substanceuse for 1A
#current alcohol use
sub$Current.alcohol.Use. <- as.character(sub$Current.alcohol.Use.)

sub$Current.alcohol.Use.[sub$Current.alcohol.Use. == 'N/A'] <- ''
sub$Current.alcohol.Use.cat[sub$Current.alcohol.Use. == 'None'] <- '0'
sub$Current.alcohol.Use.cat[sub$Current.alcohol.Use. == 'Daily'] <- '1'
sub$Current.alcohol.Use.cat[sub$Current.alcohol.Use. == '2-3 Drinks per week'] <- '1'
sub$Current.alcohol.Use.cat[sub$Current.alcohol.Use. == 'Occasional Drink'] <- '1'

sub$Current.alcohol.Use.cat <- as.integer(sub$Current.alcohol.Use.cat)
sub$Substance.Use <- ifelse(sub$drug_use == "current" | sub$drug_use == "past" & 
                                  sub$Current.alcohol.Use.cat >= 1, 
                                c("1"), c("0")) 

#convert from character into factor
sub$Substance.Use <- factor(sub$Substance.Use)

#smoking current cat
sub$Has.the.subject.ever.smoked. <- as.character(sub$Has.the.subject.ever.smoked.)

sub$smoking.current[sub$Has.the.subject.ever.smoked. == 'No'] <- '0'
sub$smoking.current[sub$Has.the.subject.ever.smoked. == 'Yes (Currently)'] <- '1'
sub$smoking.current[sub$Has.the.subject.ever.smoked. == 'Yes (Past Smoker)'] <- '0'

sub$smoking.current <- as.factor(sub$smoking.current)

#merge 1A and 1B2 substance.use and smoking together
a_subset <- sub %>%
  select(Participants, Substance.Use, smoking.current)

merge <- join(a_subset, b2_subset, type="full")

#write to file and fix the participant names
# write.csv(merge, "1A_1B2_substance.csv")

#merge with aldex metadata and write to file
aldex$smoking.current <- NULL
aldex$Substance.Use <- NULL

aldex2 <- join(aldex, merge, type="full")
# write.csv(aldex2, "Aldex_metadata_1A_1B2_v2.csv")

#aldex for smoking.current and substance.use
meta <- meta %>% select(Participants, smoking.current, Substance.Use, Nugent.score)
