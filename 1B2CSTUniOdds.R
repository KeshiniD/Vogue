#call data
total <- read.csv(file.path("1B2metabac_condensed.csv"))
colnames(total)

                                         
                                   
[10] "BV..number.of.episodes.2.months."               
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



#CSTI 
mylogit <- glm(formula = CSTI ~ probiotics.2.months, data=total, 
               family = binomial(link = "logit"))
summary(mylogit)