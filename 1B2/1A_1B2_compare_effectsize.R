#effect size for 1A, 1B2 comparison data: 

#load dataset
newdata <- read.csv("Aldex_metadata_1A_1B2_v2.csv")

#list of variables #make sure to convert to factor
factors <- c("X.Non..Prescription..y.1..n.0.", 
             "Vaginal.intercourse.in.past.48.hours..y.1..n.0.", "Freq.oral.sex.cat", 
             "Freq.anal.sex.cat", "Freq.sex.toy.use.cat", "Abnormal.discharge.2wks", 
             "Abnormal.odor.2wks", "Irritation.Discomfort.2wks", 
             "Other.Symptoms.2wks", "Abnormal.discharge.48hrs", 
             "Abnormal.odor.48hrs", "Irritation.Discomfort.48hrs", 
             "Other.Symptoms.48hrs", "Genwarts.ever", 
             "Number.partners.in.past.year.cat", "contraception.H", 
             "contraception.B.M", "contraception.C.IUD", "Substance.Use", 
             "Ethnicity.cat", "Tampon.Use.cat", "Presence.Symptoms.2wks", 
             "Presence.Symptoms.48hrs", "Chlamydia.ever", "Contraception.none", 
             "UTI.ever", "Trich.ever", "GenHerpes.ever", "Pregnancy.cat", 
             "smoking.current", "Symptom.pain", "Tampon.use.1mth", "CST", 
             "condoms.48h", "Feminine.products", "Feminine.products.48hrs") 

#factors
df <- data.frame(var = c(), pval = c(), phi = c(), cramer = c())

df_list_CST_factors <- lapply(factors, function(factor)  {
  
  cat("variable: ", factor, "\n")
  
  formula <- paste0("~study_arm + ", factor)
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
