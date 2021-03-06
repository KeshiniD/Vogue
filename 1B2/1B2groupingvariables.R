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
total$Preg.livebirth.ever<- NULL

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
total$Eukaryote <- NULL
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
                                       
#sxpain
summary(total$How.often.pain.experienced.during.vaginal.intercourse.percentage)
#condense to yes and no
total$Symptom.pain <- ifelse(total$How.often.pain.experienced.during.vaginal.intercourse.percentage > 0, 
                                c("1"), c("0")) 
#convert Substance.Use from character into factor
total$Symptom.pain <- factor(total$Symptom.pain)
summary(total$Symptom.pain)

#remove %variables and duplicates
total$How.often.pain.experienced.during.vaginal.intercourse.percentage <-  NULL
total$any.sx.pain <-  NULL
total$sx.pain.50.over <-  NULL
total$sx.pain.100 <-  NULL

#Contraception
summary(total$Contraception)
#condense categories (none-S, B, H and IUD): y-n categories
summary(total$contraception.H) #keep
summary(total$contraception.B.M) #keep
summary(total$contraception.C.IUD)#keep
#make new 'no contraception category
total$Contraception.none <- ifelse(total$Contraception=='None' | total$Contraception=='S', 
                             c("1"), c("0")) 
#convert Contraception.none from character into factor
total$Contraception.none <- factor(total$Contraception.none)
summary(total$Contraception.none)

#remove the remaining categories
total$Contraception <- NULL
total$Contraception.cat <- NULL
total$contraception.S.S <- NULL
total$contraception.S.P <- NULL
total$contraception.B.F <- NULL
total$HContr.Progestin.pill <- NULL
total$HContr.Combination.pill <- NULL
total$HContr.nuvaring <- NULL
total$HContr.mirena <- NULL
total$HContr.depoprovera <- NULL
total$HContr.orthoevra <- NULL
total$contr_type <- NULL

#Also remove duplicate genital infection history                                
total$Bac.STI.ever <- NULL
total$Herpes.ever <- NULL

#Symptoms #keep
summary(total$Presence.Symptoms.2wks)
summary(total$Abnormal.discharge.2wks)
summary(total$Abnormal.odor.2wks)
summary(total$Irritation.Discomfort.2wks)  
summary(total$Other.Symptoms.2wks)
summary(total$Presence.Symptoms.48hrs)  
summary(total$Abnormal.discharge.48hrs)
summary(total$Abnormal.odor.48hrs)  
summary(total$Irritation.Discomfort.48hrs)
summary(total$Other.Symptoms.48hrs)                                         

#keep condoms 48hrs and probiotics 2mths
summary(total$condoms.48h)
summary(total$probiotics.2.months)

#bundle LMP and tampon use: tampons used in past month
summary(total$Tampon.Use)
total$days.since.LMP
#menses within 30 days, do they use tampon every period (exclusively or partly)
total$Tampon.use.1mth <- ifelse(total$days.since.LMP <30 & 
                                  total$Tampon.Use =='every period, exclusively' | 
                                  total$Tampon.Use =='every period, part of the time', 
                                   c("1"), c("0")) 
#convert Tampon.use.1mth from character into factor
total$Tampon.use.1mth <- factor(total$Tampon.use.1mth)
summary(total$Tampon.use.1mth)

#remove duplicate information
total$days.since.LMP <- NULL
total$weeks.since.LMP <- NULL
total$weeks.since.LMP.cat <- NULL

#CSTs; need these outcome variables
summary(total$CST)
summary(total$CSTI)
summary(total$CSTII)
summary(total$CSTIII)
summary(total$CSTIVA)
summary(total$CSTIVC)
summary(total$CSTIVD)

#write condensed groups to file
write.csv(total, "1B2metabac_condensed.csv")

###############################################################################
#Dec-18-2015
#further group variables into 2 categories (binomial)

#call for data
data <- read.csv(file = "1B2metabac_condensed.csv")

############
#Age
#combine 30-39 with 40+
data$Age.cat[data$Age.cat > 3] <- "3" #reassinging 40+ to 30s cat
#convert Age.cat to factor
data$Age.cat <- as.factor(data$Age.cat)

###########
#BMI 
#Two categories; compare underweight to normal and overweight/obese to normal

#Underweight and Normal weight
data$BMI.under.cat[data$BMI < 18.5] <- "1" #underweight
data$BMI.under.cat[data$BMI >= 18.5 & data$BMI <=24.9] <- "2" #normal weight
data$BMI.under.cat[data$BMI >= 25] <- "" #overweight/obese

#convert BMI.cat from character into factor
data$BMI.under.cat <- factor(data$BMI.under.cat) 

#OVerweight/Obese and Normal weight
data$BMI.over.cat[data$BMI < 18.5] <- "" #underweight
data$BMI.over.cat[data$BMI >= 18.5 & data$BMI <=24.9] <- "2" #normal weight
data$BMI.over.cat[data$BMI >= 25] <- "3" #overweight/obese

#convert BMI.cat from character into factor
data$BMI.over.cat <- factor(data$BMI.over.cat) 

###########
#Ethnicity 
#combine Other, Aboriginal, and Asian
data$Ethnicity.cat[data$Ethnicity=='Caucasian'] <- '1'
data$Ethnicity.cat[data$Ethnicity=='Asian'] <- '2'
data$Ethnicity.cat[data$Ethnicity=='Aboriginal'] <- '2'
data$Ethnicity.cat[data$Ethnicity=='Other (Arab)'] <- '2'
data$Ethnicity.cat[data$Ethnicity=='Other (Haida/Scottish)'] <- '2'
#convert Ethnicity.cat from character into factor
data$Ethnicity.cat <- factor(data$Ethnicity.cat)

############
#BV and Yeast
#turn these into categories; 1-3 vs 4+ as per protocol def of RBV

#BV (2 months)
data$BV.2.months.cat[data$BV..number.of.episodes.2.months. <= 3] <- "1" #1-3 epsidoes
data$BV.2.months.cat[data$BV..number.of.episodes.2.months. >= 4] <- "2" #4+ episodes

#convert BV.2.months.cat from character into factor
data$BV.2.months.cat <- factor(data$BV.2.months.cat)

#BV (year)
data$BV.year.cat[data$BV..number.of.episodes.year. <= 3] <- "1" #1-3 epsidoes
data$BV.year.cat[data$BV..number.of.episodes.year. >= 4] <- "2" #4+ episodes

#convert BV.year.cat from character into factor
data$BV.year.cat <- factor(data$BV.year.cat)

#BV (lifetime)
data$BV.lifetime.cat[data$BV..number.of.episodes.lifetime. <= 3] <- "1" #1-3 epsidoes
data$BV.lifetime.cat[data$BV..number.of.episodes.lifetime. >= 4] <- "2" #4+ episodes

#convert BV.lifetime.cat from character into factor
data$BV.lifetime.cat <- factor(data$BV.lifetime.cat)


#Yeast
#Yeast (2 months)
data$Yeast.2.months.cat[data$Yeast..2months. <= 3] <- "1" #1-3 epsidoes
data$Yeast.2.months.cat[data$Yeast..2months. >= 4] <- "2" #4+ episodes

#convert Yeast.2.months.cat from character into factor
data$Yeast.2.months.cat <- factor(data$Yeast.2.months.cat)

#Yeast (year)
data$Yeast.year.cat[data$Yeast..year. <= 3] <- "1" #1-3 epsidoes
data$Yeast.year.cat[data$Yeast..year. >= 4] <- "2" #4+ episodes

#convert Yeast.year.cat from character into factor
data$Yeast.year.cat <- factor(data$Yeast.year.cat)

#Yeast (lifetime)
data$Yeast.lifetime.cat[data$Yeast..lifetime. <= 3] <- "1" #1-3 epsidoes
data$Yeast.lifetime.cat[data$Yeast..lifetime. >= 4] <- "2" #4+ episodes

#convert Yeast.lifetime.cat from character into factor
data$Yeast.lifetime.cat <- factor(data$Yeast.lifetime.cat)

####################
#Frequency of Tampon Use
#needs to be character to reassign value
data$Tampon.Use.cat <- mapply(as.character, data$Tampon.Use)
#create categories (never-ever cats)
data$Tampon.Use.cat[data$Tampon.Use.cat=='never'] <- '1'#never used
data$Tampon.Use.cat[data$Tampon.Use.cat=='sometimes but not for every period'] <- '2'#used ever
data$Tampon.Use.cat[data$Tampon.Use.cat=='every period, part of the time'] <- '2'
data$Tampon.Use.cat[data$Tampon.Use.cat=='every period, exclusively'] <- '2'

#convert Tampon.Use.cat from character into factor
data$Tampon.Use.cat <- factor(data$Tampon.Use.cat)

#####################
#change to never-ever cats. 
##Cat. for number of sexual partners in the last year
data$Number.partners.in.past.year.cat[data$Number.partners.in.past.year <= 0] <- "0" # 0 partners
data$Number.partners.in.past.year.cat[data$Number.partners.in.past.year >= 1] <- "1"#1 or more partners

#convert Number.partners.in.past.year.cat from character into factor
data$Number.partners.in.past.year.cat <- factor(data$Number.partners.in.past.year.cat)

#####################
#Frequency of Oral Sex
#create categories (change to never-ever cats)
#data$Freq.oral.sex.cat[data$Freq.oral.sex.cat=='never'] <- '0'
data$Freq.oral.sex.cat[data$Freq.oral.sex.cat >0] <- '1' #need only run this

#convert Freq.oral.sex.cat from character into factor
data$Freq.oral.sex.cat <- factor(data$Freq.oral.sex.cat)

######################
#Substance use
#change to never-ever cats
data$Substance.Use[data$Substance.Use >0] <- '1' #emcompass those who use past or currently

#convert Substance.Use from character into factor
data$Substance.Use <- factor(data$Substance.Use)

#write new groupings into file
write.csv(data, "1B2metabac_condensedv2.csv")
