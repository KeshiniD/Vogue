#load datasets
hiv <- read.csv(file="Vogue1B_HIVdata.csv")
hpv <- read.csv(file= "HPVinHIV_types.csv")
data <- read.csv(file="Vogue1B_collection2.csv")

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

#hpv2 is metadata dataset we want
#make sure both dataset have same particiapnts; remove participants 55, 57
rownames(hpv2) <- hpv2[,1]
hpv2[,1] <- NULL
hpv2 <- as.data.frame(t(hpv2))
