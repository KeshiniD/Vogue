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