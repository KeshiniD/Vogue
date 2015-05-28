#installed packages for data analysis
install.packages("vegan", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)
install.packages("entropart", dependencies = TRUE)
install.packages("epitools", dependencies = TRUE)
install.packages("ggtree", dependencies = TRUE)
browseVignettes("ggtree")#for doc

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

# if have random NA for participants
data <-  data[c(1:11), ]

#fixed headers
names(data)[names(data)== "X"] <- "Participants"

#rename funtion does not work with spaces unless quoted
dplyr::rename()
data <- dplyr::rename(data, Participants = X, 
                       #"Lactobacillus crispatus" = Lactobacillus.crispatus, 
                       #"Lactobacillus iners" = Lactobacillus.iners, 
                       #"Lactobacillus gasseri" = Lactobacillus.gasseri, 
                       #"Lactobacillus jensenii" = Lactobacillus.jensenii, 
                       #"Gardnerella vaginalis Group C" = Gardnerella.vaginalis.Group.C, 
                       #"Gardnerella vaginalis Group A" = Gardnerella.vaginalis.Group.A,
                       #"Gardnerella vaginalis Group B" = Gardnerella.vaginalis.Group.B,
                       #"Gardnerella vaginalis Group D" = Gardnerella.vaginalis.Group.D,
                       "Megasphaera.sp.genomosp.type.1" = Megasphaera.sp..genomosp..type.1, 
                       #"Escherichia coli" = Escherichia.coli,"Prevotella timonensis" = Prevotella.timonensis, 
                       "Clostridia.sp.BVAB2" = Clostridia.sp...probably.BVAB2., 
                       "Clostridium.genomosp.BVAB3" = Clostridium.genomosp..BVAB3, 
                       #"Atopobium vaginae" = Atopobium.vaginae, "Other Clostridia" = Other.Clostridia, 
                       #"Other Bacteroidetes" = Other.Bacteroidetes, "Other Proteobacteria" = Other.Proteobacteria,
                       #"Other Actinobacteria" = Other.Actinobacteria, "Other Firmicutes" = Other.Firmicutes, 
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
##cannot use renamed columns for this function if spaces exist in renamed col
data2 <-
  gather(data, key = 'Bacteria', value = 'Counts', Lactobacillus.crispatus, Lactobacillus.iners, Lactobacillus.gasseri, 
         Lactobacillus.jensenii, Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.A, 
         Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.D, 
         Megasphaera.sp.genomosp.type.1, Escherichia.coli, Prevotella.timonensis, Clostridia.sp.BVAB2, 
         Clostridium.genomosp.BVAB3, Atopobium.vaginae, Anaerobes, Other.Clostridia, 
         Other.Bacteroidetes, Other.Proteobacteria, Other.Actinobacteria, Other.Firmicutes, Other)

# subset of bacterial data and percentages
vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Bacteria, Counts) %>%
  mutate(Species.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#make stacked plot
barplot(as.matrix(vmb)) #not exactly what I want
prop <-  prop.table(vmb, margin = 2)
barplot(prop, col = heat.colors(length(rownames(prop))), width = 2) #heatchart but doesn't work

#need to rearrange data and then plot with following code
##code works great!
ggplot(data = vmb, aes(x = Participants, y = Species.Percentage, fill = Bacteria)) + 
  geom_bar(stat = "identity") + coord_flip() + ylab("Species Proportion")
# need to fix colours, category names and legend size

#Shannon Diversity
library(vegan)

#works but shouldn't use numbers
vmb2 <- data[2:22] # just have the bacterial species
H <- diversity(vmb2) #shannon for individuals
View(H)

vmb_untidy2 <- vmb_untidy[3] # Shannon for entire cohort
H2 <- diversity(vmb_untidy2)
View(H2)

#with dplyr
H2 <- data %>% #individuals
    select(Lactobacillus.crispatus, Lactobacillus.iners, Lactobacillus.gasseri, 
         Lactobacillus.jensenii, Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.A, 
         Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.D, 
         Megasphaera.sp.genomosp.type.1, Escherichia.coli, Prevotella.timonensis, Clostridia.sp.BVAB2, 
         Clostridium.genomosp.BVAB3, Atopobium.vaginae, Anaerobes, Other.Clostridia, 
         Other.Bacteroidetes, Other.Proteobacteria, Other.Actinobacteria, Other.Firmicutes, Other) 
F <- diversity(H2)
View(F)
F2 <- dplyr::rename(F, ShannonDiveristy = X)#need to set X

#cohort
#first need to make new table with total counts for each bacterial species
H3 <- data2 %>% 
  select (Bacteria, Counts) %>% #WORKED!!!! for cohort
  group_by(Bacteria) %>%
  summarize(TotalCounts = sum(Counts)) %>%
  select (TotalCounts) #stay in for diversity,can remove to see bac
F3 <- diversity(H3)
View(F3)

#Pielou's eveness
J <- F/log(specnumber(H2)) # for individuals
View(J)

J2 <- F3/log(specnumber(H3, MARGIN = 1)) #not working for entire cohort
#specnumber want number of species and we can manually enter 21 to get J2
#fixed it with below code, and can be used for diversity
View(J2)

# want to sum up all bacteria in one row, with bacteria as columns
# can then use this in specnumber() and then pielou for cohort
h4 <- colSums(H2)
H5 <-  t(h4)

#rarefraction curves
H2 <- data %>% #same code as above, need in this format
select(Lactobacillus.crispatus, Lactobacillus.iners, Lactobacillus.gasseri, 
       Lactobacillus.jensenii, Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.A, 
       Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.D, 
       Megasphaera.sp.genomosp.type.1, Escherichia.coli, Prevotella.timonensis, Clostridia.sp.BVAB2, 
       Clostridium.genomosp.BVAB3, Atopobium.vaginae, Anaerobes, Other.Clostridia, 
       Other.Bacteroidetes, Other.Proteobacteria, Other.Actinobacteria, Other.Firmicutes, Other) 
##all the codes work, need to understand it
quantile(rowSums(H2))
rowSums(H2)
Srar <- rarefy(H2, min(rowSums(H2)))
View(Srar)
S2 <- rarefy(H2, 2)
View(S2)
all(rank(Srar) == rank(S2)) #same output as vegan doc
range(diversity(H2, "simp") - (S2 -1))

#plots curve but error in lengths; not as nice as rarecurve()
source("http://www.jennajacobs.org/R/rarefaction.txt")
emend.rare<-rarefaction(H2, color=TRUE, legend = TRUE)

#trying to plot; understand arguments
diversity(H2, index = "shannon", MARGIN = 1, base = exp(1)) #also calc Shannon
rarefy(H2, sample = 2, se = FALSE, MARGIN = 1) #set sample to integer (should be smaller than sample size)
rrarefy(H2, sample = 20) #set to integer
drarefy(H2, sample = 5) #set to integer
rarecurve(H2, step = 1, sample = 2, xlab = "Sample Size", ylab = "Species",
          label = TRUE, col = col, xlim=c(0,15000)) # will plot, set sample and step to integer
#need to figure out colours; done
col <- c("black", "darkred", "forestgreen", "orange", "blue", "yellow", 
         "hotpink", "red", "grey", "purple", "white")
#need to figure out legend (do not think there is one)
# can alter x-axis to see some samples more distinctly 

fisher.alpha(H2, MARGIN = 1)
specnumber(H2, MARGIN = 1) #works for H5

#Good's coverage; need to figure this out
## need entropart package

data(Paracou618) #example
Ns <- Paracou618.MC$Ns
Coverage(Ns)

Ns <- H3$TotalCounts#apply to own data
Coverage(Ns, Estimator = Turing) #Ns has to be numeric vector
Coverage(Ns) # is value correct; value could be for entire cohort

#individuals
Ns <- H2
Coverage(Ns, Estimator = Turing) #Ns has to be numeric vector
Coverage(Ns) #default Zhaung

#the way you aren't suppose to do it
vmb2 <- vmb[c(1:21), ]
vmb3 <- vmb2[2:3]
Ns <- vmb3$Counts
Coverage(Ns, Estimator = "Turing") #works

#may have to do separate manually and calculate each
f <- vmb$Participants
a <- split(vmb, f)#need to get this into a vector (the split frames)
# this subsets data based on participants :)
newdata <- vmb[ which(vmb$Participants=='Vogue1B2.1.29'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
Coverage(Ns, Estimator = "Turing") 
#insert each participant for coverage

#odds ratio
## needs epitools package
tapw <- c("Lowest", "Intermediate", "Highest") #example
outc <- c("Case", "Control")
dat <- matrix(c(2, 29, 35, 64, 12, 6),3,2,byrow=TRUE)
dimnames(dat) <- list("Tap water exposure" = tapw, "Outcome" = outc)
oddsratio(dat, rev="c")
oddsratio.midp(dat, rev="c")
oddsratio.fisher(dat, rev="c")
oddsratio.wald(dat, rev="c")
oddsratio.small(dat, rev="c")

# the zeros are probably a problem
t <- c("lacto", "no lacto")
o <- c("no", "inter", "yes")
dat <- matrix(c(64, 1601, 69, 0, 0, 0),2,3,byrow=TRUE)
dimnames(dat) <- list("Lacto presence" = t, "Outcome" = o)
oddsratio(dat, rev="c")

#adjusted odds ratio example
install.packages("PredictABEL", dependencies = TRUE)
library(PredictABEL)
data(ExampleData)
# specify column number of outcome variable
cOutcome <- 2
# specify column numbers of non-genetic predictors
cNonGenPred <- c(3:10)
# specify column numbers of non-genetic predictors that are categorical
cNonGenPredCat <- c(6:8)
# specify column numbers of genetic predictors
cGenPred <- c(11,13:16)
# specify column numbers of genetic predictors that are categorical
cGenPredCat <- c(0)
riskmodel <- fitLogRegModel(data=ExampleData, cOutcome=cOutcome,
                            cNonGenPreds=cNonGenPred, cNonGenPredsCat=cNonGenPredCat,
                            cGenPreds=cGenPred, cGenPredsCat=cGenPredCat)
#categorize variables (ie 19-25 = 1 etc.)
# obtain multivariate OR(95% CI) for all predictors of the fitted model
ORmultivariate(riskModel=riskmodel, filename="multiOR.txt")

#Contingency Tables
attach(data)
#can just give names, since used attach()
mytable <- table(data$Ethnicity,data$Martial.Status) #do it this way, and lengths dif
mytable

attach(ExampleData) # seems to work only with 0,1 categories
mytable <- table(Age, Sex)
mytable

attach(data) #works 
mytable <- table(abnormal.discharge..y.1..n.0., Use.of.douche.products..y.1..n.0.) 
mytable
