#Univariate plots based on p-value <0.1; plotting only significant variables
#SD Uni Plot
SD <- read.delim(file.path("SD_uni.txt"))

#select significant variables
SD <- SD[ which(SD$Variables=='Chlamydia.ever1'),]#not working
SD <- SD[1,]

SD$colour <- ifelse(SD$Estimate < 0, "negative","positive")
SD$hjust <- ifelse(SD$Estimate > 0, 1.3, -0.3)
ggplot(SD,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)") + 
  ggtitle("Shannon's Diversity Univariate Logistic Regression")

#merged CSTs and SD; took CSTs from 1B2CSTUniOdds (maybe Plots - Feb21-16)
zz<-join(SD, CSTIII, type="full")
zz2<-join(zz, CSTIVA, type="full")
outcome<-join(zz2, CSTIVC, type="full")

#plot
outcome$colour <- ifelse(outcome$Estimate < 0, "negative","positive")
outcome$hjust <- ifelse(outcome$Estimate > 0, 1.3, -0.3)
ggplot(outcome,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)") + 
  ggtitle("Univariate Logistic Regression")

#just CSTs combined
#merge CSTs
zz<-join(CSTIII, CSTIVA, type="full")
CST<-join(zz, CSTIVC, type="full")

#plot
CST$colour <- ifelse(CST$Estimate < 0, "negative","positive")
CST$hjust <- ifelse(CST$Estimate > 0, 1.3, -0.3)
ggplot(CST,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)") + 
  ggtitle("CST Univariate Logistic Regression")

######################################################################
#Feb-21-16
#combo plots for variables that were significant via Fishers

#load datasets
CSTI <- read.delim(file.path("CSTI_uni.txt"))
CSTIII <- read.delim(file.path("CSTIII_uni.txt"))
CSTIVA <- read.delim(file.path("CSTIVA_uni.txt"))
CSTIVC <- read.delim(file.path("CSTIVC_uni.txt"))
CSTIVD <- read.delim(file.path("CSTIVD_uni.txt"))

#rename variables
#did it within the files

#select significant variables
#select significant variables
CSTI <- CSTI[ which(CSTI$Variables=='(CSTI)BV.number.of.episodes.2.months.'),]
CSTIII <- CSTIII[ which(CSTIII$Variables=='(CSTIII)BV.number.of.episodes.2.months.'
                        | CSTIII$Variables=='(CSTIII)Abnormal.discharge.48hrs'),]
CSTIVA <- CSTIVA[ which(CSTIVA$Variables=='(CSTIVA)Yeast.number.of.episodes.lifetime.'
                        | CSTIVA$Variables=='(CSTIVA)Abnormal.odor.48hrs'
                        | CSTIVA$Variables=='(CSTIVA)Abnormal.odor.2wks'),]
CSTIVC <- CSTIVC[ which(CSTIVC$Variables=='(CSTIVC)Contraception.none'
                        | CSTIVC$Variables=='(CSTIVC)BV.number.of.episodes.2.months.'
                        | CSTIVC$Variables=='(CSTIVC)Contraception.Hormonal'
                        | CSTIVC$Variables=='(CSTIVC)Contraception.Barrier.Male'),]
CSTIVD <- CSTIVD[ which(CSTIVD$Variables=='(CSTIVD)Yeast.number.episodes.2.months.'
                        | CSTIVD$Variables=='(CSTIVD)Yeast.number.of.episodes.year.'
                        | CSTIVD$Variables=='(CSTIVD)Number.partners.in.past.year'),]

#merge CSTs
zz<-join(CSTI, CSTIII, type="full")
zz <- join(zz, CSTIVA, type='full')
zz <- join(zz, CSTIVC, type='full')
CST<-join(zz, CSTIVD, type="full")

#plot
CST$colour <- ifelse(CST$Estimate < 0, "negative","positive")
CST$hjust <- ifelse(CST$Estimate > 0, 1.3, -0.3)
ggplot(CST,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)") + 
  ggtitle("CST Univariate Logistic Regression") + 
  theme(plot.title = element_text(size=22), 
        axis.title = element_text(size=16,face="bold"), 
        axis.text = element_text(size=14, face="bold"))


