#CSTI Uni Plot
CSTI <- read.delim(file.path("CSTI_uni.txt"))

CSTI$colour <- ifelse(CSTI$Estimate < 0, "negative","positive")
CSTI$hjust <- ifelse(CSTI$Estimate > 0, 1.3, -0.3)
ggplot(CSTI,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)")

#CSTII Uni Plot
CSTII <- read.delim(file.path("CSTII_uni.txt"))

CSTII$colour <- ifelse(CSTII$Estimate < 0, "negative","positive")
CSTII$hjust <- ifelse(CSTII$Estimate > 0, 1.3, -0.3)
ggplot(CSTII,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)")

#CSTIII Uni Plot
CSTIII <- read.delim(file.path("CSTIII_uni.txt"))

CSTIII$colour <- ifelse(CSTIII$Estimate < 0, "negative","positive")
CSTIII$hjust <- ifelse(CSTIII$Estimate > 0, 1.3, -0.3)
ggplot(CSTIII,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)")

#CSTIVA Uni Plot
CSTIVA <- read.delim(file.path("CSTIVA_uni.txt"))

CSTIVA$colour <- ifelse(CSTIVA$Estimate < 0, "negative","positive")
CSTIVA$hjust <- ifelse(CSTIVA$Estimate > 0, 1.3, -0.3)
ggplot(CSTIVA,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)")

#CSTIVC Uni Plot
CSTIVC<- read.delim(file.path("CSTIVC_uni.txt"))

CSTIVC$colour <- ifelse(CSTIVC$Estimate < 0, "negative","positive")
CSTIVC$hjust <- ifelse(CSTIVC$Estimate > 0, 1.3, -0.3)
ggplot(CSTIVC,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)")

#CSTIVD Uni Plot
CSTIVD<- read.delim(file.path("CSTIVD_uni.txt"))

CSTIVD$colour <- ifelse(CSTIVD$Estimate < 0, "negative","positive")
CSTIVD$hjust <- ifelse(CSTIVD$Estimate > 0, 1.3, -0.3)
ggplot(CSTIVD,aes(Variables,Estimate,label="",hjust=hjust))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue")) + 
  coord_flip() + xlab("Variables") + ylab("Odds Ratio (log)")
