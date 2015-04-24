#installed packages for data analysis
install.packages("vegan", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)
install.packages("entropart", dependencies = TRUE)
install.packages("epitools", dependencies = TRUE)

#load packages
library(vegan)
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)
library(assertthat)
library(entropart)
library(epitools)

#loaded datasets, tsv form puts them in nice tables
metadata <- read.delim(file.path("completemetadataR.tsv"))
data <- read.delim(file.path("completesummaryR.tsv"))

#fixed headers
names(data)[names(data)=="X"] <- "Participants" 

#rename function does not work with spaces unless quoted
dplyr::rename
data <- dplyr::rename(data, Participants = X, 
                       "Lactobacillus crispatus" = Lactobacillus.crispatus, 
                       "Lactobacillus iners" = Lactobacillus.iners, 
                       "Lactobacillus gasseri" = Lactobacillus.gasseri, 
                       "Lactobacillus jensenii" = Lactobacillus.jensenii, 
                       "Gardnerella vaginalis Group C" = Gardnerella.vaginalis.Group.C, 
                       "Gardnerella vaginalis Group A" = Gardnerella.vaginalis.Group.A,
                       "Gardnerella vaginalis Group B" = Gardnerella.vaginalis.Group.B,
                       "Gardnerella vaginalis Group D" = Gardnerella.vaginalis.Group.D,
                       "Megasphaera sp. genomosp. type 1" = Megasphaera.sp..genomosp..type.1, 
                       "Escherichia coli" = Escherichia.coli,"Prevotella timonensis" = Prevotella.timonensis, 
                       "Clostridia sp. BVAB2" = Clostridia.sp...probably.BVAB2., 
                       "Clostridium genomosp. BVAB3" = Clostridium.genomosp..BVAB3, 
                       "Atopobium vaginae" = Atopobium.vaginae, "Other Clostridia" = Other.Clostridia, 
                       "Other Bacteroidetes" = Other.Bacteroidetes, "Other Proteobacteria" = Other.Proteobacteria,
                       "Other Actinobacteria" = Other.Actinobacteria, "Other Firmicutes" = Other.Firmicutes, 
                       "Nugent Score" = Nugent.score, "Amsel's Criteria" = Amsels, 
                       "Marital Status" = Marital.Status, "Highest Education Level" = Highest.Education.Level, 
                       "Current or chronic conditions" = Current.or.chronic.conditions...y.1..n.0., 
                       "History of Genital Infections" = Genital.Infections..y.1..n.0., 
                       "Number of BV episodes (past 2 months)" = BV..number.of.episodes.2.months., 
                       "Number of BV episodes (past year)" = BV..number.of.episodes.year., 
                       "Number of BV episodes (lifetime)" = BV..number.of.episodes.lifetime., 
                       "Number of Yeast episodes (past two months)" = Yeast..2months., 
                       "Number of Yeast episodes (past year)" = Yeast..year., 
                       "Number of Yeast episodes (lifetime)" = Yeast..lifetime.,
                       "Number of UTI episodes (past two months)" = UTI..2.months., 
                       "Number of UTI episodes (past year)" = UTI..year., 
                       "Number of UTI episodes (lifetime)" = UTI..lifetime., "Trichomoniasis" = Trich,
                       "Number of Genital Warts episodes (past two months)" = Genital.Warts..2months., 
                       "Number of Gential warts episodes (past year)" = Genital.Warts..year., 
                       "Number of Genital Warts episodes (lifetime)" = Genital.Warts..lifetime., 
                       "History of Genital Herpes" = Genital.Herpes, 
                       "Number of Chlamydia episodes (past two months)" = Chlamydia..2.months., 
                       "Number of Chlamydia episodes (past year)" = Chlamydia..year., 
                       "Number of Chlamydia episodes (lifetime)" = Chlamydia..lifetime., 
                       "History of Gonorrhea" = Gonorrhea, "History of Syphilis" = Syphillis, 
                       "Antimicrobial Use (past 3 months)" = Antimicrobial.Use..y.1..n.0., 
                       "(Non)Prescription Use (past 2 months)" = X.Non..Prescription..y.1..n.0., 
                       "Frequency of Menstrual Period" = Freq.of.Menstrual.Period, "Tampon Usage" = Tampon.Use, 
                       "Pregnancy History (Gravida)" = Pregnancy.History..g., 
                       "Pregnancy History (Term)" = Pregnancy.History..term., 
                       "Pregnancy History (Spontaneous Abortion)" = Pregnancy.History..sa., 
                       "Pregnancy History (Terminated Abortion)" = Pregnancy.History..ta., 
                       "Pregnancy History (Livebirth)" = Pregnancy.History..l., 
                       "Pregnancy History (Preterm)" = Pregnancy.History..p., 
                       "Presence of Symptoms" = Symptoms..y.1..n.0., 
                       "Abnormal Discharge" = abnormal.discharge..y.1..n.0., 
                       "Abnormal Odor" = abnormal.odor..y.1..n.0., 
                       "Irritation/Discomfort" = irritation.or.discomfort..y.1..n.0., 
                       "Other symptoms" = other, 
                       "How often pain experienced during vaginal intercourse" = pain.during.vaginal.intercourse..how.often., 
                       "Douche Product Usage" = Use.of.douche.products..y.1..n.0., 
                       "Use in past 48 hours" = Used.in.the.past.48.hours, 
                       "Feminine Hygenie Product Usage" = Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0., 
                       "Form of Contraception" = Form.of.contraception, "Sexual Partners" = Sexual.Partners, 
                       "NUmber of partners (past two months)" = Number.partners.in.past.2.months, 
                       "NUmber of partners (past year)" = Number.partners.in.past.year, 
                       "Vaginal Intercourse (past 48 hours)" = Vaginal.intercourse.in.past.48.hours..y.1..n.0., 
                       "Frequency of Oral Sex" = Freq.oral.sex, 
                       "Oral Sex (past 48 hours)" = oral.sex.in.past.48.hours..y.1..n.0., 
                       "Frequency of Anal Sex" = Freq.anal.sex, 
                       "Anal Sex (past 48 hours)" = anal.sex.in.past.48.hours..y.1..n.0., 
                       "Frequency of Sex Toy Use" = Freq.sex.toy.use, 
                       "Sex Toy Use (past 48 hours)" = use.in.past.48.hours..y.1..n.0., 
                       "Illicit Substance Use" = use.of.drugs..y.1..n.0., "Alcohol Use" = alcohol.use..y.1..n.0., 
                       "Smoking (Current or Past)" = smoker..current.or.in.past...y.1..n.0.) 

#make plot for Other bacteria
ggplot(data, aes(x=Participants, y=Other)) + 
  geom_bar(stat = "identity") + aes(fill=Participants) + coord_flip()

#subset of the data
vmb <- data[1:22] # selects certain columns, [1:22,] would select for rows
#should avoid using numbers though

# new column for bacteria and counts
##cannot use renamed columns for this function, rename after...have to figure it out
vmb <-
  gather(data, key = 'Bacteria', value = 'Counts', Lactobacillus.crispatus, Lactobacillus.iners, Lactobacillus.gasseri, 
         Lactobacillus.jensenii, Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.A, 
         Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.D, 
         Megasphaera.sp..genomosp..type.1, Escherichia.coli, Prevotella.timonensis, Clostridia.sp...probably.BVAB2., 
         Clostridium.genomosp..BVAB3, Atopobium.vaginae, Anaerobes, Other.Clostridia, 
         Other.Bacteroidetes, Other.Proteobacteria, Other.Actinobacteria, Other.Firmicutes, Other)

# subset of bacterial data and percentages
vmb2 <- tbl_df(vmb) %>% # finally got the percentages correct
  group_by(X) %>%
  select(X, Bacteria, Counts) %>%
  mutate(PC = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(X)

#make stacked plot
barplot(as.matrix(vmb)) #not exactly what I want
prop <-  prop.table(vmb, margin = 2)
barplot(prop, col = heat.colors(length(rownames(prop))), width = 2) #heatchart but doesn't work

#need to rearrange data and then plot with following code
ggplot(data = vmb2, aes(x = X, y = Counts, fill = Bacteria)) + 
  geom_bar(stat = "identity") + coord_flip()

#same code as previously mentioned and works great!
ggplot(data = vmb2, aes(x = X, y = PC, fill = Bacteria)) + 
  geom_bar(stat = "identity") + coord_flip() + ylab("Species Proportion")
# need to fix colours, category names and legend size

#Shannon Diversity
library(vegan)
vmb2 <- data[2:22] # just have the bacterial species
H <- diversity(vmb2) #shannon for individuals
View(H)

vmb_untidy2 <- vmb_untidy[3] # Shannon for entire cohort
H2 <- diversity(vmb_untidy2)
View(H2)

#Pielou's eveness
J <- H/log(specnumber(vmb2)) # for individuals
View(J)

J2 <- H2/log(specnumber(vmb_untidy2)) #not working for entire cohort
View(J2)

#rarefraction curves

#Good's coverage
install.packages("entropart", dependencies = TRUE)
library(entropart)
data(Paracou618) #example
Ns <- Paracou618.MC$Ns
Coverage(Ns)

Coverage(vmb_untidy2) #trying to apply it to own data
View(Ns)

vmbtbl2 <- vmbtbl[2:3] 
Coverage(vmbtbl2, Estimator = Turing)

#odds ratio
install.packages("epitools", dependencies = TRUE)
library(epitools)

