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

#loaded datasets
metadata <- read.delim(file.path("completemetadataR.txt"))
data <- read.delim(file.path("completesummaryR.txt"))

#fixed headers
names(data)[names(data)=="X"] <- "Participants" #rename function does not work for quoted names

#make plot for bacteria
ggplot(data, aes(x=Participants, y=Other)) + 
  geom_bar(stat = "identity") + aes(fill=Participants) + coord_flip()

#subset of the data
vmb <- data[1:22] # selects certain columns, [1:22,] would select for rows

#make stacked plot
barplot(as.matrix(vmb)) #not exactly what I want
prop <-  prop.table(vmb, margin = 2)
barplot(prop, col = heat.colors(length(rownames(prop))), width = 2) #heatchart but doesn't work

#need to rearrange data and then plot with following code
ggplot(data = vmb_untidy, aes(x = Participants, y = Counts, fill = Bacteria)) + 
  geom_bar(stat = "identity") + coord_flip()

#arrange
vmb_untidy <-
  gather(vmb, key = 'Bacteria', value = 'Counts', Lactobacillus.crispatus, Lactobacillus.iners, Lactobacillus.gasseri, 
         Lactobacillus.jensenii, Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.A, 
         Gardnerella.vaginalis.Group.B, Gardnerella.vaginalis.Group.D, 
         Megasphaera.sp..genomosp..type.1, Escherichia.coli, Prevotella.timonensis, Clostridia.sp...probably.BVAB2., 
         Clostridium.genomosp..BVAB3, Atopobium.vaginae, Anaerobes, Other.Clostridia, 
         Other.Bacteroidetes, Other.Proteobacteria, Other.Actinobacteria, Other.Firmicutes, Other)
vmb_untidy

# for percentage
vmb_percentage <- vmb_untidy$Participants %>%
  aggregate(Counts = Counts/sum(Counts), FUN = sum)

vmb_percentage <- vmb_untidy %>% # arranged by participants
  arrange(Participants)  %>% 
  summarize (PercentageCounts = Counts/sum(Counts))

vmbtbl <- tbl_df(vmb_untidy) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Bacteria, Counts) %>%
  mutate(PC = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#same code as previously mentioned and works great!
ggplot(data = vmbtbl, aes(x = Participants, y = PC, fill = Bacteria)) + 
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

#rename
dplyr::rename
dplyr::rename(df, people = x)