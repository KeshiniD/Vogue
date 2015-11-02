#make_otu_table and then this is a phylo object
#and can be used in phyloseq, ggphylo and ggtree
install.packages("qiimer", dependencies = TRUE)
library(qiimer)

data <- read.delim(file.path("completesummaryR.tsv"))

data <-  data[c(1:11), ]

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

vmb2 <- tbl_df(data) %>% #subset
  select (Lactobacillus.crispatus, Lactobacillus.iners, Lactobacillus.gasseri, 
          Lactobacillus.jensenii, Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.A, 
          Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.D, 
          Megasphaera.sp.genomosp.type.1, Escherichia.coli, Eukaryote, Prevotella.timonensis, Clostridia.sp.BVAB2, 
          Clostridium.genomosp.BVAB3, Atopobium.vaginae, Anaerobes, Other.Clostridia, 
          Other.Bacteroidetes, Other.Proteobacteria, Other.Actinobacteria, Other.Firmicutes, Other)
m_matrix <- data.matrix(vmb2)

a <- make_otu_table(m_matrix, sample_ids = TRUE)
m_matrix
#may need FASTA form

