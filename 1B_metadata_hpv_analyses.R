#load packages
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)

#load dataset
total <- read.csv(file="1B_grouped.csv")

#convert appropriate variables into factor, integers
total$t06 <- factor(total$t06)

"                                  "t11"                                 
[3] "t16"                                  "t18"                                 
[5] "t26"                                  "t31"                                 
[7] "t33"                                  "t34"                                 
[9] "t35"                                  "t39"                                 
[11] "t40"                                  "t42"                                 
[13] "t44"                                  "t45"                                 
[15] "t51"                                  "t52"                                 
[17] "t53"                                  "t54"                                 
[19] "t56"                                  "t58"                                 
[21] "t59"                                  "t61"                                 
[23] "t62"                                  "t66"                                 
[25] "t67"                                  "t68"                                 
[27] "t69"                                  "t70"                                 
[29] "t71"                                  "t72"                                 
[31] "t73"                                  "t81"                                 
[33] "t82"                                  "t83"                                 
[35] "t84"                                  "t89"                                 
[37] "Number.of.Different.HPV.Types"        "CST"                                 
[39] "Age.cat"                              "BMI.under.cat"                       
[41] "BMI.over.cat"                         "Ethnicity.cat"                       
[43] "Ethnicity2.cat"                       "bv_life"                             
[45] "bv_infecttotal_1yr"                   "bv_infecttotal_2mo"                  
[47] "BV.ever"                              "Yeast.ever"                          
[49] "UTI.ever"                             "Trich.ever"                          
[51] "Condyloma.ever"                       "GenHerpes.ever"                      
[53] "Chlamydia.ever"                       "Gonorrhea.ever"                      
[55] "Syphillis.ever"                       "Presence.Symptoms.2wks"              
[57] "Presence.Symptoms.48hrs"              "Symptom.pain"                        
[59] "oralsxfrequency.cat"                  "analsxfrequency.cat"                 
[61] "sextoyfrequency.cat"                  "sexpartner1yr.cat"                   
[63] "Contraception.H"                      "Contraception.B.M"                   
[65] "Contraception.IUD"                    "Contraception.none"                  
[67] "condoms.48h"                          "Pregnancy.cat"                       
[69] "Feminine.products"                    "Feminine.products.48hrs"             
[71] "Tampon.Use.cat"                       "days.since.LMP"                      
[73] "Tampon.use.1mth"                      "smoking.current"                     
[75] "druguse"                              "substanceuse"                        
[77] "Med.Duration"                         "Is.the.patient.antiretroviral.naive."
[79] "HIV.Clade...Result"                   "Likely.mode.of.HIV.acquisition"      
[81] "Duration.of.HIV.Infection."           "CD4.Nadir."                          
[83] "Highest.VL.Ever.."                    "CD4."                                
[85] "VL..copies.mL.."                      "HCV.Antibody...Result"               
[87] "HCV.PCR...Result"                     "HBV.sAb...Result"                    
[89] "HBV.sAg...Result"                     "HBV.cAb...Result"                    
[91] "nugent_score_result"                  "sexpartner"                          
[93] "contramethnotactive___1"              "abnormaldischarge2wk"                
[95] "abnormaldischarge48"                  "abnormalodor2wk"                     
[97] "abnormalodor48"                       "irritationdiscomfort2wk"             
[99] "irritationdiscomfort48"               "vaginalsymptomother2wk"              
[101] "vaginalsymptomother48" 






#t06, t11, 34, 39, 44, 69, 82
#have only 1 factor level (0 participants have these types)
