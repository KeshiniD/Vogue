#clusters info
clust7 <- read.csv("data_clust_7_v2.csv")
clust7np <- read.csv("data_clust_7_minus_pap_v2.csv")
sd <- read.csv("virome_individual_SD_all.csv")
clust7 <- dplyr::rename(clust7, study_id = X)
clust7np <- dplyr::rename(clust7np, study_id = X)
sd <- dplyr::rename(sd, study_id = Participants)

#merge with virome data
meta <- read.csv("viromeall_metadata_full.csv")
total <- join(clust7, clust7np, type="full")
total <- join(total, meta, type="full")
total <- join(total, sd, type="full")

#format and clean
total$X <- NULL
total$X.1 <- NULL
total$Med.Duration[is.na(total$Med.Duration)] <- 0                  
total$Duration.of.HIV.Infection.[is.na(total$Duration.of.HIV.Infection.)] <- 0                  
total$Highest.VL.Ever..[is.na(total$Highest.VL.Ever..)] <- 0                  
total$VL..copies.mL..[is.na(total$VL..copies.mL..)] <- 0                  
total$Feminine.products.48hrs[is.na(total$Feminine.products.48hrs)] <- 0
total$Group <- factor(total$Group)
total$Group_minus_pap <- factor(total$Group_minus_pap)

#loop for Fisher's and ANOVA tests for CST(viral) as dependent

#lists of variables
factors <- c("Age.cat", "BMI.under.cat", 
             "BMI.over.cat", "Ethnicity.cat", "Ethnicity2.cat", "Yeast.ever", 
             "UTI.ever", "Trich.ever", "Chlamydia.ever", "Condyloma.ever",
             "GenHerpes.ever", "Gonorrhea.ever", "Syphillis.ever", 
             "Presence.Symptoms.2wks", "Presence.Symptoms.48hrs", "Symptom.pain", 
             "oralsxfrequency.cat", "analsxfrequency.cat", "sextoyfrequency.cat", 
             "sexpartner1yr.cat", "sexpartner2mo.cat", "Contraception.H", 
             "Contraception.B.M", "Contraception.IUD", "Contraception.none", 
             "condoms.48h", "Pregnancy.cat", "Feminine.products", 
             "Feminine.products.48hrs", "Tampon.Use.cat",  
             "Tampon.use.1mth", "smoking.current", "druguse", "substanceuse", 
             "nugent_score_result", "sexpartner", "contramethnotactive___1", 
             "abnormaldischarge2wk", "abnormaldischarge48","abnormalodor2wk", 
             "abnormalodor48", "irritationdiscomfort2wk", "irritationdiscomfort48", 
             "vaginalsymptomother2wk", "rxdrug", 
             "antimicrodrug", "vaginalintercourse48hr", "t16", "t18", 
             "t26", "t33", "t35", "t42", "t45", 
             "t51", "t52", "t53", "t54", "t56", "t61", "t62", "t66", 
             "t67", "t68", "t70", "t71", "t72", "t73", "t81", "t83", 
             "t84", "t89", "Is.the.patient.antiretroviral.naive.", 
             "HIV.Clade...Result", "Likely.mode.of.HIV.acquisition", 
             "HCV.Antibody...Result", "HCV.PCR...Result", "HBV.sAb...Result", 
             "HBV.sAg...Result", "HBV.cAb...Result", "study_arm")

contins <- c("age", "bmi", "bv_life", "bv_infecttotal_1yr", "bv_infecttotal_2mo",
             "days.since.LMP", "Number.of.Different.HPV.Types", "Med.Duration",
             "Duration.of.HIV.Infection.", "CD4.Nadir.", "Highest.VL.Ever..", "CD4.",
             "VL..copies.mL..", "ShannonsDiversity")

#Fisher's loop
df <- data.frame(var = c(), pval = c(), phi = c(), cramer = c())

df_list_Group_minus_pap_factors <- lapply(factors, function(factor)  {
  
  cat("variable: ", factor, "\n")
  
  formula <- paste0("~", factor, " + Group_minus_pap")
  result <- xtabs(formula, data = total)
  
  fisher <- fisher.test(result)
  assoc <- assocstats(result)
  
  pval <- fisher$p.value
  phi <- assoc$phi
  cramer <- assoc$cramer
  
  row <- data.frame(var = factor, pval = pval, phi = phi, cramer = cramer)
  row
})  
df_list_Group_minus_pap_factors <- do.call(rbind, df_list_Group_minus_pap_factors)

###################################
#ANOVA loop
df <- data.frame(var = c(), pval = c(), Fvalue = c())

df_list_Group_minus_pap_contin <- lapply(contins, function(contin)  {
  
  cat("variable: ", contin, "\n")
  
  result <- summary(aov(total[[contin]] ~total$Group_minus_pap))
  
  pval <- result[[1]]$`Pr(>F)`[1]
  Fvalue <- result[[1]]$`F value`[1]
  
  row <- data.frame(var = contin, pval = pval, Fvalue = Fvalue)
  row
})  
df_list_Group_minus_pap_contin <- do.call(rbind, df_list_Group_minus_pap_contin)

#######################################################
#for those with more than 2 factor levels

#####################################################
#loop for correlation and t-tests for Shannon's Diversity as dependent

#list of variables
factors <- c("Age.cat", "BMI.under.cat", 
             "BMI.over.cat", "Ethnicity.cat", "Yeast.ever", 
             "UTI.ever", "Trich.ever", "Chlamydia.ever", "Condyloma.ever",
             "GenHerpes.ever", "Gonorrhea.ever", 
             "Presence.Symptoms.2wks", "Presence.Symptoms.48hrs", "Symptom.pain", 
             "oralsxfrequency.cat", "analsxfrequency.cat", "sextoyfrequency.cat", 
             "sexpartner1yr.cat", "sexpartner2mo.cat", "Contraception.H", 
             "Contraception.B.M", "Contraception.none", 
             "condoms.48h", "Pregnancy.cat", "Feminine.products", 
             "Feminine.products.48hrs", "Tampon.Use.cat",  
             "Tampon.use.1mth", "smoking.current", "substanceuse", 
             "contramethnotactive___1", 
             "abnormaldischarge2wk", "abnormaldischarge48","abnormalodor2wk", 
             "abnormalodor48", "irritationdiscomfort2wk", "irritationdiscomfort48", 
             "rxdrug", 
             "antimicrodrug", "vaginalintercourse48hr", "t16", "t33", "t42",  
             "t51", "t54", "t56", "t61", "t62", "t66", 
             "t67", "t70", "t81", "t84", "HBV.sAg...Result")

contins <- c("age", "bmi", "bv_life", "bv_infecttotal_1yr", "bv_infecttotal_2mo",
             "days.since.LMP", "Number.of.Different.HPV.Types", "Med.Duration",
             "Duration.of.HIV.Infection.", "CD4.Nadir.", "Highest.VL.Ever..", "CD4.",
             "VL..copies.mL..", "ShannonsDiversity")

#correlation loop
df <- data.frame(var = c(), pval = c(), pearson = c())

df_list_SD_contin <- lapply(contins, function(contin)  {
  
  cat("variable: ", contin, "\n")
  
  result <- cor.test(~total$ShannonsDiversity + total[[contin]], method = c("pearson"))
  
  pval <- result$p.value
  pearson <- result$estimate
  
  row <- data.frame(var = contin, pval = pval, pearson = pearson)
  row
})  
df_list_SD_contin <- do.call(rbind, df_list_SD_contin)

#############
#t.test loop

df <- data.frame(var = c(), pval = c(), cohend = c())

df_list_SD_factor <- lapply(factors, function(factor)  {
  
  cat("variable: ", factor, "\n")
  
  ttest <- paste0("t.test(ShannonsDiversity~", factor, ", data = total)")
  result_ttest <- eval(parse(text = ttest))
  
  cohend <- paste0("cohen.d(ShannonsDiversity~", factor, ", data = total)")
  result_cohend <- eval(parse(text = cohend))
  
  pval <- result_ttest$p.value
  cohend <- result_cohend$estimate
  
  row <- data.frame(var = factor, pval = pval, cohend = cohend)
  row
})  
df_list_SD_factor <- do.call(rbind, df_list_SD_factor)

#####################################################################################
#for those with more than 2 factor levels

#lists of variables
factors <- c("Group", "Group_minus_pap", "CST", "Ethnicity2.cat", "Syphillis.ever", 
             "Contraception.IUD", "druguse", "nugent_score_result", "sexpartner", 
             "vaginalsymptomother2wk", "t18", "t26", "t35", "t45", "t52", "t53", 
             "t68", "t71", "t72", "t73", "t83", "t89", 
             "Is.the.patient.antiretroviral.naive.", "HIV.Clade...Result", 
             "Likely.mode.of.HIV.acquisition", "HCV.Antibody...Result", 
             "HCV.PCR...Result", "HBV.sAb...Result", "HBV.cAb...Result", "study_arm")

###################################
#ANOVA loop
df <- data.frame(var = c(), pval = c(), Fvalue = c())

df_list_SD_factor_more <- lapply(factors, function(factor)  {
  
  cat("variable: ", factor, "\n")
  
  result <- summary(aov(total$ShannonsDiversity ~total[[factor]]))
  
  pval <- result[[1]]$`Pr(>F)`[1]
  Fvalue <- result[[1]]$`F value`[1]
  
  row <- data.frame(var = factor, pval = pval, Fvalue = Fvalue)
  row
})  
df_list_SD_factor_more <- do.call(rbind, df_list_SD_factor_more)


####################################################################################
#new BV cats
total <- read.csv(file = "viromeall_metadata_full.csv")

total$BV.2mths.cat[total$bv_infecttotal_2mo > 0] <- "1" #1+
total$BV.2mths.cat[total$bv_infecttotal_2mo <= 0] <- "0" #0
total$BV.2mths.cat <- factor(total$BV.2mths.cat)

total$BV.year.cat[total$bv_infecttotal_1yr > 2] <- "2" #3+
total$BV.year.cat[total$bv_infecttotal_1yr > 0 & total$bv_infecttotal_1yr <= 2] <- "1" #1-2
total$BV.year.cat[total$bv_infecttotal_1yr <= 0] <- "0" #0
total$BV.year.cat <- factor(total$BV.year.cat)

total$BV.life.cat[total$bv_life > 9] <- "3" #10+
total$BV.life.cat[total$bv_life > 2 & total$bv_life <= 9] <- "2" #3-9
total$BV.life.cat[total$bv_life > 0 & total$bv_life <= 2] <- "1" #1-2
total$BV.life.cat[total$bv_life <= 0] <- "0" #0
total$BV.life.cat <- factor(total$BV.life.cat)

#SD for 2+ factors
summary(aov(total$ShannonsDiversity ~total$BV.year.cat))
summary(aov(total$ShannonsDiversity ~total$BV.life.cat))

#sd for 2 levels
t.test(ShannonsDiversity~BV.2mths.cat, data = total)
cohen.d(ShannonsDiversity~BV.2mths.cat, data = total)

#######################
#Virome Groups for new cats
a <- xtabs(~BV.year.cat + Group , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~BV.life.cat + Group , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~BV.2mths.cat + Group , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~CST + Group , data = total)
fisher.test(a)
assocstats(a)

#####################
#Virome Groups minus pap for new cats
a <- xtabs(~BV.year.cat + Group_minus_pap , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~BV.life.cat + Group_minus_pap , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~BV.2mths.cat + Group_minus_pap , data = total)
fisher.test(a)
assocstats(a)

a <- xtabs(~CST + Group_minus_pap , data = total)
fisher.test(a)
assocstats(a)

