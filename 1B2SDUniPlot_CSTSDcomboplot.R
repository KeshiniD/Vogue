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

#merged CSTs and SD; took CSTs from 1B2CSTUniOdds
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