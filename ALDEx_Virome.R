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

# meta <- meta[c(1:21),] #1A
# meta <- meta[c(30:54),] #1B
# meta <- meta[c(22:29),] #1B2
# 
# ####
# #omit columns with only 1 factor level
# #1A
# meta$Gonorrhea.ever <- NULL
# meta$Syphillis.ever <- NULL
# meta$analsxfrequency.cat <- NULL
# meta$abnormaldischarge2wk <- NULL
# meta$abnormaldischarge48 <- NULL
# meta$abnormalodor2wk <- NULL
# meta$abnormalodor48 <- NULL
# meta$vaginalsymptomother2wk <- NULL
# meta$vaginalsymptomother48 <- NULL
# meta$antimicrodrug <- NULL
# meta <- meta[c(1:47)]
# 
# #1B
# meta$Contraception.IUD <- NULL
# meta$sexpartner <- NULL
# meta$vaginalsymptomother48 <- NULL
# meta$study_arm <- NULL
# meta$t06 <- NULL
# meta$t11 <- NULL
# meta$t31 <- NULL
# meta$t34 <- NULL
# meta$t39 <- NULL
# meta$t40 <- NULL
# meta$t44 <- NULL
# meta$t58 <- NULL
# meta$t59 <- NULL
# meta$t69 <- NULL
# meta$t82 <- NULL
# 
# #1B2
# meta$Trich.ever <- NULL
# meta$GenHerpes.ever <- NULL
# meta$Gonorrhea.ever <- NULL
# meta$Syphillis.ever <- NULL
# meta$Presence.Symptoms.2wks <- NULL
# meta$sexpartner1yr.cat <- NULL
# meta$Contraception.IUD <- NULL
# meta$substanceuse <- NULL
# meta$vaginalsymptomother2wk <- NULL
# meta$vaginalsymptomother48 <- NULL
# meta <- meta[c(1:47)]


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
