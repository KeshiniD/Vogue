#call data
total <- read.csv(file.path("1B2metabac_condensed.csv"))
#variable names
colnames(total)

#need to be factors if wish to treat like categories
total$Nugent.score.cat <- factor(total$Nugent.score.cat)
total$Amsels.cat <- factor(total$Amsels.cat)
total$Age.cat <- factor(total$Age.cat)
total$BMI.cat <- factor(total$BMI.cat)
total$Ethnicity.cat <- factor(total$Ethnicity.cat)
total$Antimicrobial.Use..y.1..n.0. <- factor(total$Antimicrobial.Use..y.1..n.0.)
total$X.Non..Prescription..y.1..n.0. <- factor(total$X.Non..Prescription..y.1..n.0.)
total$Freq.of.Menstrual.Period.cat <- factor(total$Freq.of.Menstrual.Period.cat)
total$Tampon.Use.cat <- factor(total$Tampon.Use.cat)
total$Vaginal.intercourse.in.past.48.hours..y.1..n.0. <- factor(total$Vaginal.intercourse.in.past.48.hours..y.1..n.0.)
total$Presence.Symptoms.2wks <- factor(total$Presence.Symptoms.2wks)
total$Abnormal.discharge.2wks <- factor(total$Abnormal.discharge.2wks)
total$Abnormal.odor.2wks <- factor(total$Abnormal.odor.2wks)
total$Irritation.Discomfort.2wks <- factor(total$Irritation.Discomfort.2wks)
total$Other.Symptoms.2wks <- factor(total$Other.Symptoms.2wks)
total$Presence.Symptoms.48hrs <- factor(total$Presence.Symptoms.48hrs)
total$Abnormal.discharge.48hrs <- factor(total$Abnormal.discharge.48hrs)
total$Abnormal.odor.48hrs <- factor(total$Abnormal.odor.48hrs)
total$Irritation.Discomfort.48hrs <- factor(total$Irritation.Discomfort.48hrs)
total$Other.Symptoms.48hrs <- factor(total$Other.Symptoms.48hrs)
total$contraception.H <- factor(total$contraception.H)
total$contraception.B.M <- factor(total$contraception.B.M)
total$contraception.C.IUD <- factor(total$contraception.C.IUD)
total$condoms.48h <- factor(total$condoms.48h)
total$probiotics.2.months <- factor(total$probiotics.2.months)
total$CST <- factor(total$CST)
total$CSTI <- factor(total$CSTI)
total$CSTII <- factor(total$CSTII)
total$CSTIII <- factor(total$CSTIII)
total$CSTIVA <- factor(total$CSTIVA)
total$CSTIVC <- factor(total$CSTIVC)
total$CSTIVD <- factor(total$CSTIVD)
total$Sexual.Partners.cat <- factor(total$Sexual.Partners.cat)
total$Freq.oral.sex.cat <- factor(total$Freq.oral.sex.cat)
total$Freq.anal.sex.cat <- factor(total$Freq.anal.sex.cat)
total$Freq.sex.toy.use.cat <- factor(total$Freq.sex.toy.use.cat)
total$Chlamydia.ever <- factor(total$Chlamydia.ever)
total$Genwarts.ever <- factor(total$Genwarts.ever)
total$Number.partners.in.past.year.cat <- factor(total$Number.partners.in.past.year.cat)
total$UTI.ever <- factor(total$UTI.ever)
total$Trich.ever <- factor(total$Trich.ever)
total$GenHerpes.ever <- factor(total$GenHerpes.ever)
total$Pregnancy.cat <- factor(total$Pregnancy.cat)
total$Feminine.products <- factor(total$Feminine.products)
total$Feminine.products.48hrs <- factor(total$Feminine.products.48hrs)
total$Substance.Use <- factor(total$Substance.Use)
total$smoking.current <- factor(total$smoking.current)
total$Symptom.pain <- factor(total$Symptom.pain)
total$Contraception.none <- factor(total$Contraception.none)
total$Tampon.use.1mth <- factor(total$Tampon.use.1mth)

#CSTI 
mylogit <- glm(formula = CSTI ~ Ethnicity.cat, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)


[10] ""               
[11] "BV..number.of.episodes.year."                   
[12] "BV..number.of.episodes.lifetime."               
[13] "Yeast..2months."                                
[14] "Yeast..year."                                   
[15] "Yeast..lifetime."                               
[16] "Antimicrobial.Use..y.1..n.0."                   
[17] "X.Non..Prescription..y.1..n.0."                 
[19] "Vaginal.intercourse.in.past.48.hours..y.1..n.0."
[22] "Age.cat"                                        
[23] "BMI.cat"                                        
[24] "Ethnicity.cat"                                  
[25] "Tampon.Use.cat"                                 
[26] "Sexual.Partners.cat"                            
[27] "Freq.oral.sex.cat"                              
[28] "Freq.anal.sex.cat"                              
[29] "Freq.sex.toy.use.cat"                           
[30] "Presence.Symptoms.2wks"                         
[31] "Abnormal.discharge.2wks"                        
[32] "Abnormal.odor.2wks"                             
[33] "Irritation.Discomfort.2wks"                     
[34] "Other.Symptoms.2wks"                            
[35] "Presence.Symptoms.48hrs"                        
[36] "Abnormal.discharge.48hrs"                       
[37] "Abnormal.odor.48hrs"                            
[38] "Irritation.Discomfort.48hrs"                    
[39] "Other.Symptoms.48hrs"                           
[40] "Chlamydia.ever"                                 
[41] "Genwarts.ever"                                  
[42] "Number.partners.in.past.year.cat"               
[43] "contraception.H"                                
[44] "contraception.B.M"                              
[45] "contraception.C.IUD"                            
[46] "condoms.48h"                                    
[47] "probiotics.2.months"                            
[55] "UTI.ever"                                       
[56] "Trich.ever"                                     
[57] "GenHerpes.ever"                                 
[58] "Pregnancy.cat"                                  
[59] "Feminine.products"                              
[60] "Feminine.products.48hrs"                        
[61] "Substance.Use"                                  
[62] "smoking.current"                                
[63] "Symptom.pain"                                   
[64] "Contraception.none"                             
[65] "Tampon.use.1mth"  
