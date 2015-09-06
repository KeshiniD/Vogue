#Multivariate plots
#CSTI multi Plot
CSTI <- read.delim(file.path("CSTI_multi.txt"))

CSTI$colour <- ifelse(CSTI$Estimate < 0, "negative","positive")
CSTI$hjust <- ifelse(CSTI$Estimate > 0, 1.3, -0.3)
ggplot(CSTI,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)")

#CSTII multi Plot
CSTII <- read.delim(file.path("CSTII_multi.txt"))

CSTII$colour <- ifelse(CSTII$Estimate < 0, "negative","positive")
CSTII$hjust <- ifelse(CSTII$Estimate > 0, 1.3, -0.3)
ggplot(CSTII,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)")

#CSTIII multi Plot
CSTIII <- read.delim(file.path("CSTIII_multi.txt"))

CSTIII$colour <- ifelse(CSTIII$Estimate < 0, "negative","positive")
CSTIII$hjust <- ifelse(CSTIII$Estimate > 0, 1.3, -0.3)
ggplot(CSTIII,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)")

#CSTIVA multi Plot
CSTIVA <- read.delim(file.path("CSTIVA_multi.txt"))

CSTIVA$colour <- ifelse(CSTIVA$Estimate < 0, "negative","positive")
CSTIVA$hjust <- ifelse(CSTIVA$Estimate > 0, 1.3, -0.3)
ggplot(CSTIVA,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)")

#CSTIVC multi Plot
CSTIVC<- read.delim(file.path("CSTIVC_multi.txt"))

CSTIVC$colour <- ifelse(CSTIVC$Estimate < 0, "negative","positive")
CSTIVC$hjust <- ifelse(CSTIVC$Estimate > 0, 1.3, -0.3)
ggplot(CSTIVC,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)")

#CSTIVD multi Plot
CSTIVD<- read.delim(file.path("CSTIVD_multi.txt"))

CSTIVD$colour <- ifelse(CSTIVD$Estimate < 0, "negative","positive")
CSTIVD$hjust <- ifelse(CSTIVD$Estimate > 0, 1.3, -0.3)
ggplot(CSTIVD,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)")
