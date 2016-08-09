vars_xtabs <- c("rxdrug", "antimicrodrug", "Age.cat", "BMI.under.cat", 
                "BMI.over.cat", "Ethnicity.cat", "Ethnicity2.cat", "BV.ever", 
                "Yeast.ever", "UTI.ever", "Trich.ever", "Condyloma.ever", 
                "GenHerpes.ever", "Chlamydia.ever", "Gonorrhea.ever", 
                "Syphillis.ever", "Presence.Symptoms.2wks", 
                "Presence.Symptoms.48hrs", "abnormaldischarge2wk", 
                "abnormaldischarge48", "abnormalodor2wk", "abnormalodor48", 
                "irritationdiscomfort2wk", "irritationdiscomfort48", 
                "vaginalsymptomother2wk", "Symptom.pain", 
                "Contraception.H", "Contraception.B.M", 
                "Contraception.none", "contramethnotactive___1", "condoms.48h", 
                "Pregnancy.cat", "oralsxfrequency.cat", "analsxfrequency.cat", 
                "sextoyfrequency.cat", "sexpartner1yr.cat", 
                "Feminine.products", "Feminine.products.48hrs", "Tampon.Use.cat", 
                "Tampon.use.1mth", "smoking.current", "druguse", "substanceuse", 
                "Is.the.patient.antiretroviral.naive.", "HIV.Clade...Result", 
                "Likely.mode.of.HIV.acquisition", "HCV.Antibody...Result", 
                "HCV.PCR...Result", "HBV.sAb...Result", "HBV.sAg...Result", 
                "HBV.cAb...Result", "CST", "nugent_score_result")
vars_glm <- c("bv_infecttotal_2mo", "bv_infecttotal_1yr", "bv_life", 
              "Med.Duration", "Duration.of.HIV.Infection.", "CD4.Nadir.", 
              "Highest.VL.Ever..", "CD4.", "VL..copies.mL..", 
              "Number.of.Different.HPV.Types")
hpv <- c("t16", "t18", "t26", "t31", "t33", "t35", "t40", "t42", "t45", "t51", 
         "t52", "t53", "t54", "t56", "t58", "t59", "t61", "t62", "t66", "t67", 
         "t68", "t70", "t71", "t72", "t73", "t81", "t83", "t84", "t89")

df <- data.frame(hpv = c(), var = c(), pval = c(), phi = c(), cramer = c())

nothing <- lapply(hpv, function(x) {
  nothing <- lapply(vars_xtabs, function(var) {
    formula <- paste0("~", x, " + ", var)
    result <- xtabs(formula, data = total)
    
    fisher <- fisher.test(result)
    assoc <- assocstats(result)
    
    pval <- fisher$p.value
    phi <- assoc$phi
    cramer <- assoc$cramer
    
    row <- data.frame(hpv = x, var = var, pval = pval, phi = phi, cramer = cramer)
    df <<- rbind(df, row)
#     cat(x, "\t", var, " (xtabs) \n")
#     cat("pvalue:\t", pval, "\n")
#     cat("phi:\t", phi, "\n")
#     cat("cramer:\t", cramer, "\n")
#     cat("\n")
  })
  
  nothing <- 
    lapply(vars_glm, function(var) {
      formula <- paste0(x, "~", var)
      mylogit <- suppressWarnings(
        glm(formula = formula, data=total, 
                     family = binomial(link = "logit"))
      )
      mylogit <- summary(mylogit)
      pval <- mylogit$coefficients[2,4]
      
      row <- data.frame(hpv = x, var = var, pval = pval, phi = NA, cramer = NA)
      df <<- rbind(df, row)
#       cat(x, "\t", var, " (glm) \n")
#       cat("pvalue:\t", pval, "\n\n")
    })
})

#write to file
#write.csv(df, "1B_hpv_analyses_univariate.csv")


######################################################################################
#July-29-16
#loop for Fisher's and ANOVA tests for CST as dependent

#lists of variables
factors <- c("Age.cat", "BMI.cat", "BMI.under.cat", "BMI.over.cat", 
             "Ethnicity.cat", "Yeast.ever", "UTI.ever", "Chlamydia.ever", "Genwarts.ever", 
             "GenHerpes.ever", "Antimicrobial.Use..y.1..n.0.", 
             "X.Non..Prescription..y.1..n.0.", "probiotics.2.months", 
             "Presence.Symptoms.2wks", "Abnormal.discharge.2wks", 
             "Abnormal.odor.2wks", "Irritation.Discomfort.2wks", 
             "Other.Symptoms.2wks", "Presence.Symptoms.48hrs", 
             "Abnormal.discharge.48hrs", "Abnormal.odor.48hrs", 
             "Irritation.Discomfort.48hrs", "Other.Symptoms.48hrs", 
             "Symptom.pain", "Vaginal.intercourse.in.past.48.hours..y.1..n.0.", 
             "Freq.oral.sex.cat", "Freq.anal.sex.cat", "Freq.sex.toy.use.cat", 
             "Sexual.Partners.cat", "Number.partners.in.past.year.cat", 
             "contraception.H", "contraception.B.M", "contraception.C.IUD", 
             "Contraception.none", "condoms.48h", "Pregnancy.cat", "Feminine.products", 
             "Feminine.products.48hrs", "Tampon.Use.cat", "Tampon.use.1mth", 
             "Substance.Use", "smoking.current")

contins <- c("BV..number.of.episodes.2.months.", 
            "BV..number.of.episodes.year.", "BV..number.of.episodes.lifetime.", 
            "Shannon.s.Diversity")

#Fisher's loop
df <- data.frame(var = c(), pval = c(), phi = c(), cramer = c())

df_list_CST_factors <- lapply(factors, function(factor)  {
 
  cat("variable: ", factor, "\n")
  
  formula <- paste0("~", factor, " + CST")
  result <- xtabs(formula, data = total)
  
  fisher <- fisher.test(result)
  assoc <- assocstats(result)
  
  pval <- fisher$p.value
  phi <- assoc$phi
  cramer <- assoc$cramer
  
  row <- data.frame(var = factor, pval = pval, phi = phi, cramer = cramer)
  row
})  
df_list_CST_factors <- do.call(rbind, df_list_CST_factors)

###################################
#ANOVA loop
df <- data.frame(var = c(), pval = c(), Fvalue = c())

df_list_CST_contin <- lapply(contins, function(contin)  {

  cat("variable: ", contin, "\n")
  
  result <- summary(aov(total[[contin]] ~total$CST))

  pval <- result[[1]]$`Pr(>F)`[1]
  Fvalue <- result[[1]]$`F value`[1]

  row <- data.frame(var = contin, pval = pval, Fvalue = Fvalue)
  row
})  
df_list_CST_contin <- do.call(rbind, df_list_CST_contin)

#####################################################
#loop for correlation and t-tests for Shannon's Diversity as dependent

#list of variables
factors <- c("Age.cat", "BMI.under.cat", "BMI.over.cat", 
             "Ethnicity.cat", "Yeast.ever", "UTI.ever", "Chlamydia.ever", "Genwarts.ever", 
             "GenHerpes.ever", "Antimicrobial.Use..y.1..n.0.", 
             "X.Non..Prescription..y.1..n.0.", "probiotics.2.months", 
             "Presence.Symptoms.2wks", "Abnormal.discharge.2wks", 
             "Abnormal.odor.2wks", "Irritation.Discomfort.2wks", 
             "Other.Symptoms.2wks", "Presence.Symptoms.48hrs", 
             "Abnormal.discharge.48hrs", "Abnormal.odor.48hrs", 
             "Irritation.Discomfort.48hrs", "Other.Symptoms.48hrs", 
             "Symptom.pain", "Vaginal.intercourse.in.past.48.hours..y.1..n.0.", 
             "Freq.oral.sex.cat", "Freq.anal.sex.cat", "Freq.sex.toy.use.cat", 
             "Number.partners.in.past.year.cat", 
             "contraception.H", "contraception.B.M", 
             "Contraception.none", "condoms.48h", "Pregnancy.cat", "Feminine.products", 
             "Feminine.products.48hrs", "Tampon.Use.cat", "Tampon.use.1mth", 
             "Substance.Use", "smoking.current")

contins <- c("BV..number.of.episodes.2.months.", 
            "BV..number.of.episodes.year.", "BV..number.of.episodes.lifetime.")

#correlation loop
df <- data.frame(var = c(), pval = c(), pearson = c())

df_list_SD_contin <- lapply(contins, function(contin)  {
  
  cat("variable: ", contin, "\n")
  
  result <- cor.test(~total$Shannon.s.Diversity + total[[contin]], method = c("pearson"))
  
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
  
  ttest <- paste0("t.test(Shannon.s.Diversity~", factor, ", data = total)")
  result_ttest <- eval(parse(text = ttest))
  
  cohend <- paste0("cohen.d(Shannon.s.Diversity~", factor, ", data = total)")
  result_cohend <- eval(parse(text = cohend))
  
  pval <- result_ttest$p.value
  cohend <- result_cohend$estimate
  
  row <- data.frame(var = factor, pval = pval, cohend = cohend)
  row
})  
df_list_SD_factor <- do.call(rbind, df_list_SD_factor)

#####################################################################################
# redid for only high Nugent (4+) scores
newdata <- subset(total, Nugent.score >= 4) 
newdata$CST <- droplevels(newdata$CST) # get rid of empty levels

##
#loop for Fisher's and ANOVA tests for CST as dependent

#lists of variables
factors <- c("Age.cat", "BMI.cat", "BMI.under.cat", "BMI.over.cat", 
             "Ethnicity.cat", "Yeast.ever", "UTI.ever", "Chlamydia.ever", "Genwarts.ever", 
             "GenHerpes.ever", "Antimicrobial.Use..y.1..n.0.", 
             "X.Non..Prescription..y.1..n.0.", "probiotics.2.months", 
             "Presence.Symptoms.2wks", "Abnormal.discharge.2wks", 
             "Abnormal.odor.2wks", "Irritation.Discomfort.2wks", 
             "Other.Symptoms.2wks", "Presence.Symptoms.48hrs", 
             "Abnormal.discharge.48hrs", "Abnormal.odor.48hrs", 
             "Irritation.Discomfort.48hrs", "Other.Symptoms.48hrs", 
             "Symptom.pain", "Vaginal.intercourse.in.past.48.hours..y.1..n.0.", 
             "Freq.oral.sex.cat", "Freq.anal.sex.cat", "Freq.sex.toy.use.cat", 
             "Sexual.Partners.cat", "Number.partners.in.past.year.cat", 
             "contraception.H", "contraception.B.M", "contraception.C.IUD", 
             "Contraception.none", "condoms.48h", "Pregnancy.cat", "Feminine.products", 
             "Feminine.products.48hrs", "Tampon.Use.cat", "Tampon.use.1mth", 
             "Substance.Use", "smoking.current")

contins <- c("BV..number.of.episodes.2.months.", 
             "BV..number.of.episodes.year.", "BV..number.of.episodes.lifetime.", 
             "Shannon.s.Diversity")

#Fisher's loop
df <- data.frame(var = c(), pval = c(), phi = c(), cramer = c())

df_list_CST_factors <- lapply(factors, function(factor)  {
  
  cat("variable: ", factor, "\n")
  
  formula <- paste0("~", factor, " + CST")
  result <- xtabs(formula, data = newdata)
  
  fisher <- fisher.test(result)
  assoc <- assocstats(result)
  
  pval <- fisher$p.value
  phi <- assoc$phi
  cramer <- assoc$cramer
  
  row <- data.frame(var = factor, pval = pval, phi = phi, cramer = cramer)
  row
})  
df_list_CST_factors <- do.call(rbind, df_list_CST_factors)

###################################
#ANOVA loop
df <- data.frame(var = c(), pval = c(), Fvalue = c())

df_list_CST_contin <- lapply(contins, function(contin)  {
  
  cat("variable: ", contin, "\n")
  
  result <- summary(aov(newdata[[contin]] ~newdata$CST))
  
  pval <- result[[1]]$`Pr(>F)`[1]
  Fvalue <- result[[1]]$`F value`[1]
  
  row <- data.frame(var = contin, pval = pval, Fvalue = Fvalue)
  row
})  
df_list_CST_contin <- do.call(rbind, df_list_CST_contin)

#####do CST and BMI.cat with SD in aov
summary(aov(total$Shannon.s.Diversity ~total$CST))
summary(aov(total$Shannon.s.Diversity ~total$BMI.cat))


#####################################################
#loop for correlation and t-tests for Shannon's Diversity as dependent

#list of variables
factors <- c("Age.cat", "BMI.under.cat", "BMI.over.cat", 
             "Ethnicity.cat", "Yeast.ever", "UTI.ever", 
             "Chlamydia.ever", "Genwarts.ever", 
             "GenHerpes.ever", "Antimicrobial.Use..y.1..n.0.", 
             "X.Non..Prescription..y.1..n.0.", "probiotics.2.months", 
             "Abnormal.discharge.2wks", 
             "Abnormal.odor.2wks", "Irritation.Discomfort.2wks", 
             "Other.Symptoms.2wks", "Presence.Symptoms.48hrs", 
             "Abnormal.discharge.48hrs", "Abnormal.odor.48hrs", 
             "Irritation.Discomfort.48hrs", "Other.Symptoms.48hrs", 
             "Symptom.pain", "Vaginal.intercourse.in.past.48.hours..y.1..n.0.", 
             "Freq.oral.sex.cat", "Freq.anal.sex.cat", "Freq.sex.toy.use.cat", 
             "Number.partners.in.past.year.cat", 
             "contraception.H", "contraception.B.M", 
             "Contraception.none", "condoms.48h", "Pregnancy.cat", "Feminine.products", 
             "Feminine.products.48hrs", "Tampon.Use.cat", "Tampon.use.1mth", 
             "Substance.Use", "smoking.current")

contins <- c("BV..number.of.episodes.2.months.", 
             "BV..number.of.episodes.year.", "BV..number.of.episodes.lifetime.")

#correlation loop
df <- data.frame(var = c(), pval = c(), pearson = c())

df_list_SD_contin <- lapply(contins, function(contin)  {
  
  cat("variable: ", contin, "\n")
  
  result <- cor.test(~newdata$Shannon.s.Diversity + newdata[[contin]], method = c("pearson"))
  
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
  
  ttest <- paste0("t.test(Shannon.s.Diversity~", factor, ", data = newdata)")
  result_ttest <- eval(parse(text = ttest))
  
  cohend <- paste0("cohen.d(Shannon.s.Diversity~", factor, ", data = newdata)")
  result_cohend <- eval(parse(text = cohend))
  
  pval <- result_ttest$p.value
  cohend <- result_cohend$estimate
  
  row <- data.frame(var = factor, pval = pval, cohend = cohend)
  row
})  
df_list_SD_factor <- do.call(rbind, df_list_SD_factor)

#####do CST and BMI.cat with SD in aov
summary(aov(newdata$Shannon.s.Diversity ~newdata$CST))
summary(aov(newdata$Shannon.s.Diversity ~newdata$BMI.cat))
