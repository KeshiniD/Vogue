DO_PARALLEL <- FALSE # TRUE = run 4 core, FALSE = run single core
FAST_VERSION <- FALSE # TRUE = mc=2, FALSE = run to completion(mc=128)

#load packages
library(ALDEx2)
library(plyr)
library(dplyr)
library(tidyr)

#load data

#Aldex for DNA_RNA_phage_all
viral <- read.csv("DNA_RNA_phage_viral_species_all.csv")
meta <- read.csv("viromeall_metadata_full.csv") #renamed manually to match viral

#variables need to be factors; maybe? try integer and see
# set the rownames as the taxa names
viral$X <- NULL
row.names(viral) <- viral[, 1]
viral <- viral[, -1]
viral[is.na(viral)] <- 0
viral <- viral[,order(colnames(viral))]
meta$X <- NULL
meta$X.1 <- NULL
meta$Med.Duration[is.na(meta$Med.Duration)] <- 0                  
meta$Duration.of.HIV.Infection.[is.na(meta$Duration.of.HIV.Infection.)] <- 0                  
meta$Highest.VL.Ever..[is.na(meta$Highest.VL.Ever..)] <- 0                  
meta$VL..copies.mL..[is.na(meta$VL..copies.mL..)] <- 0                  
meta$Feminine.products.48hrs[is.na(meta$Feminine.products.48hrs)] <- 0
meta <- meta %>% 
  arrange(study_id)

#
#these have one factor level
meta$t06 <- NULL
meta$t11 <- NULL
meta$t31 <- NULL
meta$t34 <- NULL
meta$t39 <- NULL
meta$t40 <- NULL
meta$t44 <- NULL
meta$t58 <- NULL
meta$t59 <- NULL
meta$t69 <- NULL
meta$t82 <- NULL
meta$vaginalsymptomother48 <- NULL
######

#Aldex
variables <- colnames(meta)
# variables <- variables[c(2,7,17)]
notfactors <- c(
  "age", "bmi", "bv_life", "bv_infecttotal_1yr", "bv_infecttotal_2mo",
  "days.since.LMP", "Number.of.Different.HPV.Types", "Med.Duration",
  "Duration.of.HIV.Infection.", "CD4.Nadir.", "Highest.VL.Ever..", "CD4.",
  "VL..copies.mL.."
)

# notfactors <- c(
#   "age", "bmi", "bv_life", "bv_infecttotal_1yr", "bv_infecttotal_2mo", 
#   "days.since.LMP"
# )

# run the aldex code on each core on your computer, should be 2-4 times faster
if (DO_PARALLEL) {
  cl <- parallel::makeCluster(4)
  parallel::clusterExport(cl = cl, varlist = c("meta", "viral", "FAST_VERSION"))
  
  mydf <- parLapply(cl, variables, function(var) {
    
    if ( var == "study_id" ) {
      return()
    }

    #make a vector that is the variable labels
    cond.edu <- meta[[var]]
    
    #run ALDEx
    if (FAST_VERSION) {
      ald.edu <- ALDEx2::aldex(reads = viral, conditions = cond.edu, test = "glm", effect = FALSE, mc.samples = 2, verbose = FALSE)
    } else {
      ald.edu <- ALDEx2::aldex(reads = viral, conditions = cond.edu, test = "glm", effect = FALSE)
    }
    
    #look at the output
    #head(ald.edu)
    #cat(var, "\t", min(ald.edu$glm.eBH), "\t", min(ald.edu$kw.eBH), "\n")
    row <- data.frame(variable = var, glm.eBH = min(ald.edu$glm.eBH), kw.eBH = min(ald.edu$kw.eBH))
    #mydf <<- rbind(mydf, row)
    row
  })
  
  mydf <- do.call(rbind, mydf)
  
} else {
 # the old slower way
  mydf <- data.frame(variable = c(), glm.eBH = c(), kw.eBH = c())
  
  lapply(variables, function(var) {
    
    if ( var == "study_id" ) {
      return()
    }
    
    #make a vector that is the variable labels
    cond.edu <- meta[[var]]
    
    #run ALDEx
    if (FAST_VERSION) {
      ald.edu <- aldex(reads = viral, conditions = cond.edu, test = "glm", effect = FALSE, mc.samples = 2, verbose = FALSE) 
    } else {
      ald.edu <- aldex(reads = viral, conditions = cond.edu, test = "glm", effect = FALSE)  
    }
    
    #look at the output
    #head(ald.edu)
    cat(var, "\t", min(ald.edu$glm.eBH), "\t", min(ald.edu$kw.eBH), "\n")
    row <- data.frame(variable = var, glm.eBH = min(ald.edu$glm.eBH), kw.eBH = min(ald.edu$kw.eBH))
    mydf <<- rbind(mydf, row)
  })
}

mydf$signif <- mydf$kw.eBH < 0.05

#################
mydf2 <- mydf
mydf2$signif <- mydf$glm.eBH < 0.05
View(mydf2)
mydf2$signif <- mydf$glm.eBH < 0.05

##################################################################################
#################################################################################
#no variables significant using all meta and viral species


#################
#repeat aldex with separated meta data by cohort
#not suppose to do that

####################################################################################
#look at variables which are significant
meta$study_arm <- factor(meta$study_arm)

#study_arm
cond.edustudy_arm <- meta$study_arm
ald.edustudy_arm <- aldex(reads = viral, conditions = cond.edustudy_arm, test = "glm", effect = FALSE)

###############
#write to file
# write.csv(ald.edustudy_arm, "Aldex_virome_studyarm.csv")

#################################################################################
# #merge the two result files
# ald <- read.csv("Aldex_virome_results.csv")
# ald2 <- read.csv("Aldex_virome_results2.csv")
# 
# ald3 <- join(ald, ald2, type="full")
# 
# #write to file
# # write.csv(ald3, "Aldex_virome_results_combined.csv")

###################################################################
#sept29-16
#want to do for family, type and groups too

##################################
#family
#Aldex for DNA_RNA_phage_all
viral2 <- read.csv("DNA_RNA_phage_viral_family_all.csv")

# set the rownames as the taxa names
viral2$X.1 <- NULL
viral2$X <- NULL
row.names(viral2) <- viral2[, 1]
viral2 <- viral2[, -1]
viral2[is.na(viral2)] <- 0
viral2 <- viral2[,order(colnames(viral2))]

#################################
#type
#family
#Aldex for DNA_RNA_phage_all
viral2 <- read.csv("viral_type_all.csv")

# set the rownames as the taxa names
viral2$X <- NULL
row.names(viral2) <- viral2[, 1]
viral2 <- viral2[, -1]
viral2[is.na(viral2)] <- 0
viral2 <- viral2[,order(colnames(viral2))]

################################
#groupings
#family
#Aldex for DNA_RNA_phage_all
viral2 <- read.csv("viral_groups_all.csv")

# set the rownames as the taxa names
viral2$X <- NULL
row.names(viral2) <- viral2[, 1]
viral2 <- viral2[, -1]
viral2[is.na(viral2)] <- 0
viral2 <- viral2[,order(colnames(viral2))]

#####################################################################################
#get median clr for each variable
#median values to define significant assoications from aldex
library(ALDEx2)
viral <- read.csv("DNA_RNA_phage_viral_species_all.csv")
meta <- read.csv("viromeall_metadata_full.csv") #renamed manually to match viral

# set the rownames as the taxa names
viral$X <- NULL
row.names(viral) <- viral[, 1]
viral <- viral[, -1]
viral[is.na(viral)] <- 0
viral <- viral[,order(colnames(viral))]
meta$X <- NULL
meta$X.1 <- NULL
meta <- meta %>% 
  arrange(study_id)

#only handles two levels; not more than one
clr <- aldex.clr(viral, mc.samples=128)

#already have nugent and bv.2mths
meta$study_arm <- factor(meta$study_arm) 

study_arm <- aldex.effect(clr, conditions = meta$study_arm, include.sample.summary = TRUE)

#write to file
# write.csv(study_arm, "Aldex_median_studyarm.csv")