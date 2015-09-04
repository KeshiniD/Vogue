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

#collapse Trich category, Trich ever
total$Trich.ever <- ifelse(total$Trich..lifetime.=='chronic', 
                         c("1"), c("0")) 
#convert Trich.ever from character into factor
total$Trich.ever <- factor(total$Trich.ever) 
#remove other TRich categories
total$Trich..2.months. <- NULL
total$Trich..year. <- NULL
total$Trich..lifetime. <- NULL

#collapse Genital Warts category, GenWarts ever
total$Genwarts.ever <- ifelse(total$Genital.Warts..lifetime. > 0, 
                         c("1"), c("0")) 
#convert Genwarts.ever from character into factor
total$Genwarts.ever <- factor(total$Genwarts.ever) 
#remove other UTI categories
total$Genital.Warts..2months. <- NULL
total$Genital.Warts..year. <- NULL
total$Genital.Warts..lifetime. <- NULL   
 
#collapse Genital Herpes category, GenHerpes ever
total$GenHerpes.ever <- ifelse(total$Genital.Herpes..lifetime. > 0, 
                              c("1"), c("0")) 
#convert GenHerpes.ever from character into factor
total$GenHerpes.ever <- factor(total$GenHerpes.ever) 
#remove other UTI categories
total$Genital.Herpes..2months. <- NULL
total$Genital.Herpes..year. <- NULL
total$Genital.Herpes..lifetime. <- NULL   
                                              
#collapse Genital Herpes category, GenHerpes ever
total$GenHerpes.ever <- ifelse(total$Genital.Herpes..lifetime. > 0, 
                               c("1"), c("0")) 
#convert GenHerpes.ever from character into factor
total$GenHerpes.ever <- factor(total$GenHerpes.ever) 
#remove other UTI categories
total$Genital.Herpes..2months. <- NULL
total$Genital.Herpes..year. <- NULL
total$Genital.Herpes..lifetime. <- NULL   

#already have chlamydia ever category
summary(total$Chlamydia.ever)
#remove other chlamydia categories
total$Chlamydia..2.months. <- NULL
total$Chlamydia..year. <- NULL
total$Chlamydia..lifetime. <- NULL   

total$Gonorrhea.ever #no levels
#remove and syphillis too
total$Gonorrhea <- NULL
total$Gonorrhea.ever <- NULL 
total$Syphillis <- NULL

#Antimicrobials
summary(total$Antimicrobial.Use..y.1..n.0.)

#Non/Prescriptions (WHAT ARE THEY USING) --> keep
summary(total$X.Non..Prescription..y.1..n.0.)
                                                  
#Menstrual Period
summary(total$Freq.of.Menstrual.Period)
#remove and the cat version
total$Freq.of.Menstrual.Period <- NULL
total$Freq.of.Menstrual.Period.cat <- NULL

#Tampon Use (MAYBE COLLAPSE)
summary(total$Tampon.Use)
                           
#Condense pregnancy into null vs multi
# multi-1, null-0
total$Pregnancy.History..g.
total$Pregnancy.cat <- ifelse(total$Pregnancy.History..g. > 1, 
                           c("1"), c("0")) 
#convert Pregnancy.cat from character into factor
total$Pregnancy.cat <- factor(total$Pregnancy.cat) 
summary(total$Pregnancy.cat)

#remove other pregnancy variables
total$Pregnancy.History..g. <- NULL
total$Pregnancy.History..p.<- NULL
total$Pregnancy.History..term.<- NULL
total$Pregnancy.History..sa.<- NULL
total$Pregnancy.History..ta.<- NULL
total$Pregnancy.History..l.<- NULL
 
#douche products and feminine wipes
summary(total$Use.of.douche.products..y.1..n.0.)
summary(total$Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0.)
#combine today variables into feminine product use
total$Feminine.products <- ifelse(total$Use.of.douche.products..y.1..n.0. == '1' |
                                    total$Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0. == '1', 
                              c("1"), c("0")) 
total$Feminine.products <- factor(total$Feminine.products) 
summary(total$Feminine.products)

#remove douche and feminine variables
total$Use.of.douche.products..y.1..n.0.<- NULL
total$Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0.<- NULL

#used feminine products in past 48 hours
summary(total$Used.in.the.past.48.hours)#douche
summary(total$Used.in.past.48.hours)#feminine wipes
#combine today variables into feminine product use
total$Feminine.products.48hrs <- ifelse(total$Used.in.the.past.48.hours == '1' |
                                    total$Used.in.past.48.hours == '1', 
                                  c("1"), c("0")) 
total$Feminine.products.48hrs <- factor(total$Feminine.products.48hrs) 
summary(total$Feminine.products.48hrs)

#remove douche and feminine variables past 48hours
total$Used.in.the.past.48.hours<- NULL
total$Used.in.past.48.hours<- NULL

#remove Sexual Partners (LATER)
summary(total$Sexual.Partners.cat)
#Remove non-cat variables
total$Sexual.Partners <- NULL
                                         
#number of sexual partners
summary(total$Number.partners.in.past.year.cat)
summary(total$Number.partners.in.past.2.months.cat)
#removing number in past 2 months
total$Number.partners.in.past.2.months<- NULL
total$Number.partners.in.past.2.months.cat<- NULL
total$Number.partners.in.past.year<- NULL

#vaginal intercourse in the past 48 hours
summary(total$Vaginal.intercourse.in.past.48.hours..y.1..n.0.)
                                
#Oralsex
summary(total$Freq.oral.sex)
#condense, weekly-twice monthly together
#needs to be character to reassign value
total$Freq.oral.sex.cat <- mapply(as.character, total$Freq.oral.sex)
#create categories
total$Freq.oral.sex.cat[total$Freq.oral.sex=='never'] <- '0'
total$Freq.oral.sex.cat[total$Freq.oral.sex=='weekly'] <- '1'
total$Freq.oral.sex.cat[total$Freq.oral.sex=='twice a month'] <- '1'
total$Freq.oral.sex.cat[total$Freq.oral.sex=='monthly'] <- '2'
total$Freq.oral.sex.cat[total$Freq.oral.sex=='other'] <- '3'

#convert Freq.oral.sex.cat from character into factor
total$Freq.oral.sex.cat <- factor(total$Freq.oral.sex.cat)
summary(total$Freq.oral.sex.cat)

#remove oral sex and in past 48 hours
total$Freq.oral.sex <- NULL
total$oral.sex.in.past.48.hours..y.1..n.0. <- NULL

#AnalSex
summary(total$Freq.anal.sex)
#condense, all categories together
#needs to be character to reassign value
total$Freq.anal.sex <- mapply(as.character, total$Freq.anal.sex)
#create categories
total$Freq.anal.sex.cat[total$Freq.anal.sex=='never'] <- '0'
total$Freq.anal.sex.cat[total$Freq.anal.sex=='weekly'] <- '1'
total$Freq.anal.sex.cat[total$Freq.anal.sex=='monthly'] <- '1'
total$Freq.anal.sex.cat[total$Freq.anal.sex=='other'] <- '1'

#convert Freq.anal.sex.cat from character into factor
total$Freq.anal.sex.cat <- factor(total$Freq.anal.sex.cat)
summary(total$Freq.anal.sex.cat)

#remove oral sex
total$Freq.anal.sex <- NULL
total$anal.sex.in.past.48.hours..y.1..n.0. <- NULL

#SexToyuse
summary(total$Freq.sex.toy.use)
#condense, all categories together
#needs to be character to reassign value
total$Freq.sex.toy.use <- mapply(as.character, total$Freq.sex.toy.use)
#create categories
total$Freq.sex.toy.use.cat[total$Freq.sex.toy.use=='never'] <- '0'
total$Freq.sex.toy.use.cat[total$Freq.sex.toy.use=='weekly'] <- '1'
total$Freq.sex.toy.use.cat[total$Freq.sex.toy.use=='monthly'] <- '1'
total$Freq.sex.toy.use.cat[total$Freq.sex.toy.use=='other'] <- '1'

#convert Freq.sex.toy.use.cat from character into factor
total$Freq.sex.toy.use.cat <- factor(total$Freq.sex.toy.use.cat)
summary(total$Freq.sex.toy.use.cat)

#remove oral sex
total$Freq.sex.toy.use <- NULL
total$use.in.past.48.hours..y.1..n.0. <- NULL

#Substance use
summary(total$use.of.drugs..y.1..n.0.)
summary(total$alcohol.use..y.1..n.0.)
#combine: never - 0, past - 1, current - 2(all alcohol drinkers are current) 
total$Substance.Use[total$use.of.drugs..y.1..n.0.=='0' & total$alcohol.use..y.1..n.0. == '0'] <- '0'
total$Substance.Use[total$use.of.drugs..y.1..n.0.=='past'] <- '1'
total$Substance.Use[total$use.of.drugs..y.1..n.0. == 'current' | total$alcohol.use..y.1..n.0. == '2/3 drinks' | 
                                total$alcohol.use..y.1..n.0. == 'occassional'] <- '2'

#convert Substance.Use from character into factor
total$Substance.Use <- factor(total$Substance.Use)
summary(total$Substance.Use)

#remove drugs and alcohol variables and their cats.
total$use.of.drugs..y.1..n.0. <- NULL
total$alcohol.use..y.1..n.0. <- NULL
total$use.of.drugs..y.1..n.0.cat <- NULL
total$alcohol.use..y.1..n.0.cat <- NULL

#smoking
summary(total$smoker..current.or.in.past...y.1..n.0.)
summary(total$smoker..current.or.in.past...y.1..n.0.cat)
#condense to currently smoking or not
total$smoking.current <- ifelse(total$smoker..current.or.in.past...y.1..n.0. == 'current', 
                                        c("1"), c("0")) 
#convert Substance.Use from character into factor
total$smoking.current <- factor(total$smoking.current)
summary(total$smoking.current)

#remove smoking and cat. 
total$smoker..current.or.in.past...y.1..n.0. <- NULL
total$smoker..current.or.in.past...y.1..n.0.cat <- NULL

#remove all bacterial species (outcome variables)
total$Actinobacteria.sp. <- NULL
total$Atopobium.vaginae <- NULL
total$Clostridia.sp..BVAB2 <- NULL
total$Clostridium.genomosp..BVAB3 <- NULL
total$Escherichia.coli <- NULL
total$Gardnerella.vaginalis.Group.A <- NULL
total$Gardnerella.vaginalis.Group.B <- NULL
total$Gardnerella.vaginalis.Group.C <- NULL
total$Gardnerella.vaginalis.Group.D <- NULL
total$Klebsiella.pneumoniae <- NULL
total$Lactobacillus.crispatus <- NULL
total$Lactobacillus.gasseri <- NULL
total$Lactobacillus.iners <- NULL
total$Lactobacillus.jensenii <- NULL
total$Megasphaera.sp..genomosp..type.1 <- NULL
total$Other.Actinobacteria <- NULL
total$Other.Bacteria <- NULL
total$Other.Bacteroidetes <- NULL
total$Other.Clostridium <- NULL
total$Other.Firmicutes <- NULL
total$Other.Lactobacillus <- NULL
total$Other.Prevotella <- NULL
total$Other.Proteobacteria <- NULL
total$Other.Streptococcus <- NULL
total$Prevotella.amnii <- NULL                                        
total$Prevotella.timonensis <- NULL
total$Streptococcus.devriesei <- NULL                                    

#keep these outcome variables in case
total$Nugent.score.cat <- NULL
total$Amsels.cat <- NULL
                                       
                                                  
[99] "How.often.pain.experienced.during.vaginal.intercourse.percentage"
[100] "Contraception"                                                   
[101] "Contraception.cat"                                               
                                           

                    
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
                                                  
[122] "Bac.STI.ever"                                                    
[123] "Herpes.ever"                                                     
                                                  
                              
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