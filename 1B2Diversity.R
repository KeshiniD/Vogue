#Shannon Diversity
library(vegan)
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)
library(entropart)
library(epitools)

#call for data
data <- read.csv(file.path("1B2.csv"))

#just have the bacterial species
bac <- data %>%
  select(Lactobacillus.crispatus, Lactobacillus.gasseri, 
         Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, 
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)

H <- diversity(bac) #shannon for individuals
View(H)

#write data to file (altered headings and called back)
write.table(H, "1B2_individual_diversity.csv", sep = ",", row.names = FALSE, quote = FALSE)
div <- read.csv(file.path("1B2_individual_diversity.csv"))

#cohort
#first need to make new table with total counts for each bacterial species
#bac counts
data2 <-
  gather(data, key = 'Bacteria', value = 'Counts', Lactobacillus.crispatus, 
         Lactobacillus.gasseri, Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, 
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)

#then use in diversity
H2 <- data2 %>% 
  select (Bacteria, Counts) %>% #WORKED!!!! for cohort
  group_by(Bacteria) %>%
  summarize(TotalCounts = sum(Counts)) %>%
  select (TotalCounts) #stay in for diversity,can remove to see bac
F2 <- diversity(H2)
View(F2)

#write data to file (altered headings and called back)
write.table(F2, "1B2_cohort_diversity.csv", sep = ",", row.names = FALSE, quote = FALSE)
cdiv <- read.csv(file.path("1B2_cohort_diversity.csv"))

#Pielou's eveness
J<- H/log(specnumber(bac)) # for individuals
View(J)

#write data to file (altered headings and called back)
write.table(J, "1B2_individual_Pielou.csv", sep = ",", row.names = FALSE, quote = FALSE)
Piel <- read.csv(file.path("1B2_individual_Pielou.csv"))

J2 <- F2/log(specnumber(H2, MARGIN = 1)) #not working for entire cohort
#specnumber want number of species and we can manually enter 21 to get J2
#fixed it with below code, and can be used for diversity
View(J2)

# want to sum up all bacteria in one row, with bacteria as columns
# can then use this in specnumber() and then pielou for cohort
h4 <- colSums(H2)
H5 <-  t(h4)
J2 <- F2/log(H5) 
View(J2)

#write data to file (altered headings and called back)
write.table(J2, "1B2_cohort_Pielou.csv", sep = ",", row.names = FALSE, quote = FALSE)
cPiel <- read.csv(file.path("1B2_cohort_Pielou.csv"))


#Good's coverage; need to figure this out for cohort
## need entropart package

#example
data(Paracou618) 
Ns <- Paracou618.MC$Ns
Coverage(Ns)

#apply to own data
#same as above
#bac counts
data2 <-
  gather(data, key = 'Bacteria', value = 'Counts', Lactobacillus.crispatus, 
         Lactobacillus.gasseri, Lactobacillus.iners, Lactobacillus.jensenii, 
         Gardnerella.vaginalis.Group.A, Gardnerella.vaginalis.Group.B, 
         Gardnerella.vaginalis.Group.C, Gardnerella.vaginalis.Group.D, 
         Actinobacteria.sp., Atopobium.vaginae, Clostridia.sp..BVAB2, 
         Clostridium.genomosp..BVAB3, Escherichia.coli, 
         Klebsiella.pneumoniae, Megasphaera.sp..genomosp..type.1, 
         Prevotella.amnii, Prevotella.timonensis, Streptococcus.devriesei, 
         Other.Actinobacteria, Other.Bacteria, Other.Bacteroidetes, 
         Other.Clostridium, Other.Firmicutes, Other.Lactobacillus, 
         Other.Prevotella, Other.Proteobacteria, Other.Streptococcus)
#total counts
H2 <- data2 %>% 
  select (Bacteria, Counts) %>% 
  group_by(Bacteria) %>%
  summarize(TotalCounts = sum(Counts)) %>%
  select (TotalCounts)

Ns <- H2$TotalCounts
Coverage(Ns, Estimator = Turing) #Ns has to be numeric vector
Coverage(Ns) # is value correct; value could be for entire cohort?

#individuals
#may have to do separate manually and calculate each
f <- data2$Participants
a <- split(data2, f)#need to get this into a vector (the split frames)
# this subsets data based on participants :)
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.01'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
a <- Coverage(Ns, Estimator = "Turing") 
#insert each participant for coverage

#Vogue 01-06
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.06'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
b <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-07
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.07'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
c <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-08
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.08'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
d <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-09
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.09'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
e <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-10
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.10'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
f <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-11
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.11'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
g <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-12
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.12'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
h <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-15
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.15'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
i <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-19
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.19'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
j <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-21
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.21'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
k <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-23
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.23'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
l <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-26
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.26'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
m <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-28
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.28'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
n <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-29
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.29'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
o <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-35
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.35'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
p <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-37
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.37'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
q <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-38
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.38'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
r <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-50
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.50'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
s <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-52
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.52'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
t <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-56
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.56'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
u <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-58
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.58'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
v <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-61
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.61'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
w <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-62
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.62'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
x <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-63
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.63'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts
y <- Coverage(Ns, Estimator = "Turing") 

#Vogue 01-64
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.64'), ]
#does this work for below coverage function? YES!
Ns <- newdata$Counts

#turn all variables into data.frame, join data.frames and write into file
a2 <- as.data.frame(a)
b2 <- as.data.frame(b)
c2 <- as.data.frame(c)
d2 <- as.data.frame(d)
e2 <- as.data.frame(e)
f2 <- as.data.frame(f)
g2 <- as.data.frame(g)
h2 <- as.data.frame(h)
i2 <- as.data.frame(i)
j2 <- as.data.frame(j)
k2 <- as.data.frame(k)
l2 <- as.data.frame(l)
m2 <- as.data.frame(m)
n2 <- as.data.frame(n)
o2 <- as.data.frame(o)
p2 <- as.data.frame(p)
q2 <- as.data.frame(q)
r2 <- as.data.frame(r)
s2 <- as.data.frame(s)
t2 <- as.data.frame(t)
u2 <- as.data.frame(u)
v2 <- as.data.frame(v)
w2 <- as.data.frame(w)
x2 <- as.data.frame(x)
y2 <- as.data.frame(y)
z2 <- as.data.frame(z)

#merge
list.of.data.frames <- cbind(a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, 
                             l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, 
                             w2, x2, y2, z2)
#edit headers
list.of.data.frames <- dplyr::rename(list.of.data.frames, Vogue1B2.01.01 = a, 
                                     Vogue1B2.01.06 = b, Vogue1B2.01.07 = c, 
                                     Vogue1B2.01.08 = d, Vogue1B2.01.09 = e, 
                                     Vogue1B2.01.10 = f, Vogue1B2.01.11 = g, 
                                     Vogue1B2.01.12 = h, Vogue1B2.01.15 = i, 
                                     Vogue1B2.01.19 = j, Vogue1B2.01.21 = k, 
                                     Vogue1B2.01.23 = l, Vogue1B2.01.26 = m, 
                                     Vogue1B2.01.28 = n, Vogue1B2.01.29 = o, 
                                     Vogue1B2.01.35 = p, Vogue1B2.01.37 = q, 
                                     Vogue1B2.01.38 = r, Vogue1B2.01.50 = s, 
                                     Vogue1B2.01.52 = t, Vogue1B2.01.56 = u, 
                                     Vogue1B2.01.58 = v, Vogue1B2.01.61 = w, 
                                     Vogue1B2.01.62 = x, Vogue1B2.01.63 = y, 
                                     Vogue1B2.01.64 = z) 
#when transpose and write to file lose 'Participant' IDs
#list.of.data.frames <- as.data.frame(t(list.of.data.frames))

#write
write.table(list.of.data.frames, "1B2_individual_Good.csv", sep = ",", row.names = FALSE, quote = FALSE)
Good <- read.csv(file.path("1B2_individual_Good.csv"))

#merge Pielous, shannons and rarefraction
a <- read.csv(file.path("1B2_individual_Pielou.csv"))
b <- read.csv(file.path("1B2richness_individual.csv"))
c <- read.csv(file.path("1B2_individual_diversity.csv"))
b$X <- NULL #remove random empty column

#merge three folders together
diversity <- cbind(a,b,c)
#remove duplicate columns
d <- diversity[ -c(3, 5) ]

#write
write.csv(d, "1B2_individual_all_diversity.csv")

#Chao estimator
#Vogue1B2.01.06
newdata <- data2[ which(data2$Participants=='Vogue1B2.01.06'), ]
d <- diversityresult(newdata, index = 'chao')
