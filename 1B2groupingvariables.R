#grouping categories
total <- read.csv(file.path("1B2metabac_v3.csv"))

#Age
summary(total$Age.cat)
#fine

#BMI (COULD COLLASPE MORE IF NEEDED)
summary(total$BMI.cat)
#combine overweight and obese
attach(total)
total$BMI.cat[BMI < 18.5] <- "1" #underweight
total$BMI.cat[BMI >= 18.5 & BMI <=24.9] <- "2" #normal weight
total$BMI.cat[BMI >= 25] <- "3" #overweight/obese
detach(total)
#convert BMI.cat from character into factor
total$BMI.cat <- factor(total$BMI.cat)                                                

#Ethnicity (COULD COLLASPE MORE IF NEEDED)
summary(total$Ethnicity.cat)
#combine Other and Aboriginal
total$Ethnicity.cat[total$Ethnicity=='Caucasian'] <- '1'
total$Ethnicity.cat[total$Ethnicity=='Asian'] <- '2'
total$Ethnicity.cat[total$Ethnicity=='Aboriginal'] <- '3'
total$Ethnicity.cat[total$Ethnicity=='Other (Arab)'] <- '3'
total$Ethnicity.cat[total$Ethnicity=='Other (Haida/Scottish)'] <- '3'
#convert Ethnicity.cat from character into factor
total$Ethnicity.cat <- factor(total$Ethnicity.cat)

#remove Martial Status, Highest.education.level, and chronic conditions
total$Marital.Status <- NULL
total$Marital.Status.cat <- NULL
total$Highest.Education.Level <- NULL
total$Highest.Education.Level.cat <- NULL
total$Current.or.chronic.conditions...y.1..n.0. <- NULL
#remove                                                       
summary(total$Genital.Infections..y.1..n.0.)    
total$Genital.Infections..y.1..n.0. <- NULL

#keep BV and Yeast

#collapse UTI category, UTI ever
total$UTI.ever <- ifelse(total$UTI..lifetime. > 0, 
                       c("1"), c("0")) 
#convert UTI.ever from character into factor
total$UTI.ever <- factor(total$UTI.ever) 
#remove other UTI categories
total$UTI..2.months. <- NULL
total$UTI..year. <- NULL
total$UTI..lifetime. <- NULL

#collapse Trich category, UTI ever
total$Trich.ever <- ifelse(total$Trich..lifetime.=='chronic', 
                         c("1"), c("0")) 
#convert Trich.ever from character into factor
total$Trich.ever <- factor(total$Trich.ever) 
#remove other TRich categories
total$Trich..2.months. <- NULL
total$Trich..year. <- NULL
total$Trich..lifetime. <- NULL
   
                                               
[25] "Genital.Warts..2months."                                         
[26] "Genital.Warts..year."                                            
[27] "Genital.Warts..lifetime."                                        
[28] "Genital.Herpes..2months."                                        
[29] "Genital.Herpes..year."                                           
[30] "Genital.Herpes..lifetime."                                       
[31] "Chlamydia..2.months."                                            
[32] "Chlamydia..year."                                                
[33] "Chlamydia..lifetime."                                            
[34] "Gonorrhea"                                                       
[35] "Syphillis"                                                       
[36] "Antimicrobial.Use..y.1..n.0."                                    
[37] "X.Non..Prescription..y.1..n.0."                                  
[38] "Freq.of.Menstrual.Period"                                        
[39] "Tampon.Use"                                                      
[40] "Pregnancy.History..g."                                           
[41] "Pregnancy.History..term."                                        
[42] "Pregnancy.History..p."                                           
[43] "Pregnancy.History..sa."                                          
[44] "Pregnancy.History..ta."                                          
[45] "Pregnancy.History..l."                                           
[46] "Use.of.douche.products..y.1..n.0."                               
[47] "Used.in.the.past.48.hours"                                       
[48] "Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0."            
[49] "Used.in.past.48.hours"                                           
[50] "Sexual.Partners"                                                 
[51] "Number.partners.in.past.2.months"                                
[52] "Number.partners.in.past.year"                                    
[53] "Vaginal.intercourse.in.past.48.hours..y.1..n.0."                 
[54] "Freq.oral.sex"                                                   
[55] "oral.sex.in.past.48.hours..y.1..n.0."                            
[56] "Freq.anal.sex"                                                   
[57] "anal.sex.in.past.48.hours..y.1..n.0."                            
[58] "Freq.sex.toy.use"                                                
[59] "use.in.past.48.hours..y.1..n.0."                                 
[60] "use.of.drugs..y.1..n.0."                                         
[61] "alcohol.use..y.1..n.0."                                          
[62] "smoker..current.or.in.past...y.1..n.0."                          
[63] "Actinobacteria.sp."                                              
[64] "Atopobium.vaginae"                                               
[65] "Clostridia.sp..BVAB2"                                            
[66] "Clostridium.genomosp..BVAB3"                                     
[67] "Escherichia.coli"                                                
[68] "Gardnerella.vaginalis.Group.A"                                   
[69] "Gardnerella.vaginalis.Group.B"                                   
[70] "Gardnerella.vaginalis.Group.C"                                   
[71] "Gardnerella.vaginalis.Group.D"                                   
[72] "Klebsiella.pneumoniae"                                           
[73] "Lactobacillus.crispatus"                                         
[74] "Lactobacillus.gasseri"                                           
[75] "Lactobacillus.iners"                                             
[76] "Lactobacillus.jensenii"                                          
[77] "Megasphaera.sp..genomosp..type.1"                                
[78] "Other.Actinobacteria"                                            
[79] "Other.Bacteria"                                                  
[80] "Other.Bacteroidetes"                                             
[81] "Other.Clostridium"                                               
[82] "Other.Firmicutes"                                                
[83] "Other.Lactobacillus"                                             
[84] "Other.Prevotella"                                                
[85] "Other.Proteobacteria"                                            
[86] "Other.Streptococcus"                                             
[87] "Prevotella.amnii"                                                
[88] "Prevotella.timonensis"                                           
[89] "Streptococcus.devriesei"                                         
[90] "Nugent.score.cat"                                                
[91] "Amsels.cat"                                                      
[92] "Age.cat"                                                         
[93] "BMI.cat"                                                         
[94] "Ethnicity.cat"                                                   
[95] "Marital.Status.cat"                                              
[96] "Highest.Education.Level.cat"                                     
[97] "Freq.of.Menstrual.Period.cat"                                    
[98] "Tampon.Use.cat"                                                  
[99] "How.often.pain.experienced.during.vaginal.intercourse.percentage"
[100] "Contraception"                                                   
[101] "Contraception.cat"                                               
[102] "Sexual.Partners.cat"                                             
[103] "Freq.oral.sex.cat"                                               
[104] "Freq.anal.sex.cat"                                               
[105] "Freq.sex.toy.use.cat"                                            
[106] "use.of.drugs..y.1..n.0.cat"                                      
[107] "alcohol.use..y.1..n.0.cat"                                       
[108] "smoker..current.or.in.past...y.1..n.0.cat"                       
[109] "Presence.Symptoms.2wks"                                          
[110] "Abnormal.discharge.2wks"                                         
[111] "Abnormal.odor.2wks"                                              
[112] "Irritation.Discomfort.2wks"                                      
[113] "Other.Symptoms.2wks"                                             
[114] "Presence.Symptoms.48hrs"                                         
[115] "Abnormal.discharge.48hrs"                                        
[116] "Abnormal.odor.48hrs"                                             
[117] "Irritation.Discomfort.48hrs"                                     
[118] "Other.Symptoms.48hrs"                                            
[119] "Preg.livebirth.ever"                                             
[120] "Chlamydia.ever"                                                  
[121] "Gonorrhea.ever"                                                  
[122] "Bac.STI.ever"                                                    
[123] "Herpes.ever"                                                     
[124] "Genwarts.ever"                                                   
[125] "Number.partners.in.past.2.months.cat"                            
[126] "Number.partners.in.past.year.cat"                                
[127] "any.sx.pain"                                                     
[128] "sx.pain.50.over"                                                 
[129] "sx.pain.100"                                                     
[130] "contraception.H"                                                 
[131] "contraception.S.S"                                               
[132] "contraception.S.P"                                               
[133] "contraception.B.M"                                               
[134] "contraception.B.F"                                               
[135] "contraception.C.IUD"                                             
[136] "HContr.Progestin.pill"                                           
[137] "HContr.Combination.pill"                                         
[138] "HContr.nuvaring"                                                 
[139] "HContr.mirena"                                                   
[140] "HContr.depoprovera"                                              
[141] "HContr.orthoevra"                                                
[142] "contr_type"                                                      
[143] "condoms.48h"                                                     
[144] "probiotics.2.months"                                             
[145] "days.since.LMP"                                                  
[146] "weeks.since.LMP"                                                 
[147] "weeks.since.LMP.cat"                                             
[148] "CST"                                                             
[149] "CSTI"                                                            
[150] "CSTII"                                                           
[151] "CSTIII"                                                          
[152] "CSTIVA"                                                          
[153] "CSTIVC"                                                          
[154] "CSTIVD"