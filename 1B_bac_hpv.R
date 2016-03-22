#relating Vogue 1B HPV data to bacterial data

# load datasets
bac <- read.csv(file = "Vogue1B_bac.csv")
hpv <- read.csv(file= "HPVinHIV_types.csv")

#load packages
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)

#select participants with HPV data available (omit those excluded from 1B)
bac <- read.csv(file = "Vogue1B_bac.csv")
hpv <- read.csv(file= "HPVinHIV_types.csv")
nums <- substring(hpv$Vogue.1B.ID, 4)
ids <- paste0("Vogue.1B.01.", nums)
bac2 <- bac[, which(colnames(bac) %in% c(ids, "X"))]

### change study ID
hpv$Vogue.1B.ID <-
  paste0("Vogue.1B.01.",
         substring(hpv$Vogue.1B.ID, nchar("01-")+1))

#select hpv types, and merge datasets together
hpv2 <- hpv %>%
  select (Vogue.1B.ID, t06, t11, t16, t18, t26, t31, t33, t34, t35, t39, t40, 
          t42, t44, t45, t51, t52, t53, t54, t56, t58, t59, t61, t62, t66, t67, 
          t68, t69, t70, t71, t72, t73, t81, t82, t83, t84, t89)    

#cannot merge datasets; because doesn't make sense the way I want to look at data

###############################################################################
#do an aldex for each bacterial species and hpv type
source("https://bioconductor.org/biocLite.R")
biocLite("ALDEx2")
library(ALDEx2)
library(plyr)
library(dplyr)
library(tidyr)

#bac2 is dataset we want
# set the rownames as the taxa names
row.names(bac2) <- bac2[, 1]
bac2 <- bac2[, -1]

#hpv2 is metadata dataset we want
#make sure both dataset have same particiapnts; remove participants 55, 57
rownames(hpv2) <- hpv2[,1]
hpv2[,1] <- NULL
hpv2 <- as.data.frame(t(hpv2))

hpv2$Vogue.1B.01.055 <- NULL
hpv2$Vogue.1B.01.057 <- NULL
hpv2 <- as.data.frame(t(hpv2))

#make types into factors
hpv2$t06 <- factor(hpv2$t06)
hpv2$t11 <- factor(hpv2$t11)
hpv2$t16 <- factor(hpv2$t16)
hpv2$t18 <- factor(hpv2$t18)
hpv2$t26 <- factor(hpv2$t26)
hpv2$t31 <- factor(hpv2$t31)
hpv2$t33 <- factor(hpv2$t33)
hpv2$t34 <- factor(hpv2$t34)
hpv2$t35 <- factor(hpv2$t35)
hpv2$t39 <- factor(hpv2$t39)
hpv2$t40 <- factor(hpv2$t40)
hpv2$t42 <- factor(hpv2$t42)
hpv2$t44 <- factor(hpv2$t44)
hpv2$t45 <- factor(hpv2$t45)
hpv2$t51 <- factor(hpv2$t51)
hpv2$t52 <- factor(hpv2$t52)
hpv2$t53 <- factor(hpv2$t53)
hpv2$t54 <- factor(hpv2$t54)
hpv2$t56 <- factor(hpv2$t56)
hpv2$t58 <- factor(hpv2$t58)
hpv2$t59 <- factor(hpv2$t59)
hpv2$t61 <- factor(hpv2$t61)
hpv2$t62 <- factor(hpv2$t62)
hpv2$t66 <- factor(hpv2$t66)
hpv2$t67 <- factor(hpv2$t67)
hpv2$t68 <- factor(hpv2$t68)
hpv2$t69 <- factor(hpv2$t69)
hpv2$t70 <- factor(hpv2$t70)
hpv2$t71 <- factor(hpv2$t71)
hpv2$t72 <- factor(hpv2$t72)
hpv2$t73 <- factor(hpv2$t73)
hpv2$t81 <- factor(hpv2$t81)
hpv2$t82 <- factor(hpv2$t82)
hpv2$t83 <- factor(hpv2$t83)
hpv2$t84 <- factor(hpv2$t84)
hpv2$t89 <- factor(hpv2$t89)

################################################################################
#ready for Aldex

#t16
#make a vector that is the variable labels
cond.edu <- hpv2$t16
#run ALDEx
ald.edu <- aldex(reads = bac2, conditions = cond.edu, test = "glm", 
                 effect = FALSE)

#look at the output
head(ald.edu)

# smallest p-values
min(ald.edu$glm.eBH) # 0.3511769
min(ald.edu$kw.eBH) #  0.7713612

#t06, t11, 34, 39, 44, 69, 82
#have only 1 factor level (0 participants have these types)

#t18
cond.edu <- hpv2$t18
#run ALDEx
ald.edu2 <- aldex(reads = bac2, conditions = cond.edu, test = "glm", 
                 effect = FALSE)

#look at the output
head(ald.edu2)

# smallest p-values
min(ald.edu2$glm.eBH) # 0.3558991
min(ald.edu2$kw.eBH) # 0.745945

#t26
cond.edu <- hpv2$t26
#run ALDEx
ald.edu3 <- aldex(reads = bac2, conditions = cond.edu, test = "glm", 
                  effect = FALSE)

#look at the output
head(ald.edu3)

# smallest p-values
min(ald.edu3$glm.eBH) #  0.7672958
min(ald.edu3$kw.eBH) # 0.6648511

#t31
cond.edu <- hpv2$t31
#run ALDEx
ald.edut31 <- aldex(reads = bac2, conditions = cond.edu, test = "glm", 
                  effect = FALSE)

#look at the output
head(ald.edut31)

# smallest p-values
min(ald.edut31$glm.eBH) # 0.3973176
min(ald.edut31$kw.eBH) # 0.7093364

#t33
cond.edu <- hpv2$t33
#run ALDEx
ald.edut33 <- aldex(reads = bac2, conditions = cond.edu, test = "glm", 
                  effect = FALSE)

#look at the output
head(ald.edut33)

# smallest p-values
min(ald.edut33$glm.eBH) # 0.759353
min(ald.edut33$kw.eBH) # 0.8370351

#t35
cond.edu <- hpv2$t35
#run ALDEx
ald.edut35 <- aldex(reads = bac2, conditions = cond.edu, test = "glm", 
                  effect = FALSE)

#look at the output
head(ald.edut35)

# smallest p-values
min(ald.edut35$glm.eBH) # 0.7475281
min(ald.edut35$kw.eBH) # 0.7207958

#t40
cond.edu <- hpv2$t40
#run ALDEx
ald.edut40 <- aldex(reads = bac2, conditions = cond.edu, test = "glm", 
                  effect = FALSE)

#look at the output
head(ald.edut40)

# smallest p-values
min(ald.edut40$glm.eBH) # 0.5860327
min(ald.edut40$kw.eBH) # 0.7518268

#t42
cond.edu <- hpv2$t42
#run ALDEx
ald.edut42 <- aldex(reads = bac2, conditions = cond.edu, test = "glm", 
                    effect = FALSE)

#look at the output
head(ald.edut42)

# smallest p-values
min(ald.edut42$glm.eBH) # 0.3908511
min(ald.edut42$kw.eBH) # 0.793962
