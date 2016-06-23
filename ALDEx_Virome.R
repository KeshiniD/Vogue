#load packages
library(ALDEx2)
library(plyr)
library(dplyr)
library(tidyr)

#load data

#Aldex for DNA_RNA_phage_all
viral <- read.csv("DNA_RNA_phage_viral_species_all.csv")
meta <- read.csv("viromeall_metadata_full.csv")

#variables need to be factors; maybe? try integer and see
# set the rownames as the taxa names
viral$X <- NULL
row.names(viral) <- viral[, 1]
viral <- viral[, -1]
meta$X <- NULL
meta$X.1 <- NULL
meta$Number.of.Different.HPV.Types[is.na(meta$Number.of.Different.HPV.Types)] <- 0                  
meta$Med.Duration[is.na(meta$Med.Duration)] <- 0                  
meta$Duration.of.HIV.Infection.[is.na(meta$Duration.of.HIV.Infection.)] <- 0                  
meta$Highest.VL.Ever..[is.na(meta$Highest.VL.Ever..)] <- 0                  
meta$VL..copies.mL..[is.na(meta$VL..copies.mL..)] <- 0                  

#Aldex
variables <- colnames(meta)
#variables <- variables[c(2,7,17)]
# notfactors <- c(
#   "age", "bmi", "bv_life", "bv_infecttotal_1yr", "bv_infecttotal_2mo", 
#   "days.since.LMP", "Number.of.Different.HPV.Types", "Med.Duration", 
#   "Duration.of.HIV.Infection.", "CD4.Nadir.", "Highest.VL.Ever..", "CD4.", 
#   "VL..copies.mL.."
# )

notfactors <- c(
  "Number.of.Different.HPV.Types", "Med.Duration", 
  "Duration.of.HIV.Infection.", "CD4.Nadir.", "Highest.VL.Ever..", "CD4.", 
  "VL..copies.mL.."
)


mydf <- data.frame(variable = c(), glm.eBH = c(), kw.eBH = c())

lapply(variables, function(var) {
  
  if ( var == "study_id" ) {
    return()
  }
  if (!(var %in% notfactors)) {
    var < as.factor(var)
  }
  
  #make a vector that is the variable labels
  cond.edu <- meta[[var]]
  
  #run ALDEx
  ald.edu <- aldex(reads = viral, conditions = cond.edu, test = "glm", effect = FALSE)
  #ald.edu <- aldex(reads = viral, conditions = cond.edu, test = "glm", effect = FALSE, mc.samples = 2, verbose = FALSE)
  
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

##################################################################################
#################################################################################
#look at variables which are significant
meta$X.Non..Prescription..y.1..n.0. <- factor(meta$X.Non..Prescription..y.1..n.0.)
meta$Vaginal.intercourse.in.past.48.hours..y.1..n.0. <- factor(meta$Vaginal.intercourse.in.past.48.hours..y.1..n.0.)

#Nugent Score
cond.eduNS <- meta$Nugent.score
ald.eduNS <- aldex(reads = bac, conditions = cond.eduNS, test = "glm", effect = FALSE)

###############
#write to file
#write.csv(ald.eduNS, "Aldex_1B2_Nugentscore.csv")

#################################################################################
