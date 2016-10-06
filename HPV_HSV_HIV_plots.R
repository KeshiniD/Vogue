#plot
#call data
everyone <- read.csv("HPV_HSV_HIV_all.csv")
vogueA <- read.csv("HPV_HSV_HIV_1A.csv")
vogueB <- read.csv("HPV_HSV_HIV_1B.csv")
vogue1b2 <- read.csv("HPV_HSV_HIV_1B2.csv")

#to order alphabetically
# a <- a[order(a$Viral_Species),]

##################################################
#all
everyone <- read.csv("HPV_HSV_HIV_all.csv")

#collapse like species
everyone <- ddply(everyone,c("Viral_Species"),numcolwise(sum)) #includes all columns

rownames(everyone) <- everyone[,1]
everyone[,1] <- NULL
everyone <- as.everyone.frame(t(everyone))
everyone[is.na(everyone)] <- 0
everyone <- add_rownames(everyone, "Participants")

#bac counts
everyone2 <-
  gather(everyone, key = 'Viral_Family', value = 'Counts', Herpesviridae, Papillomaviridae, 
         Retroviridae) 

vmb <- tbl_df(everyone2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Family, Counts) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Family) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viral_Family']]

#Dean added to order by factor levels
vmb$Viral_Family <- factor(vmb$Viral_Family, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('purple', 'blue', 'darkgoldenrod1')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Family)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

######
#1A
data <- read.csv("HPV_HSV_HIV_1A.csv")

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#bac counts
data2 <-
  gather(data, key = 'Viral_Family', value = 'Counts', Herpesviridae, Papillomaviridae, 
         Retroviridae) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Family, Counts) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Family) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viral_Family']]

#Dean added to order by factor levels
vmb$Viral_Family <- factor(vmb$Viral_Family, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('purple', 'blue', 'darkgoldenrod1')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Family)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

############
#1B
data <- read.csv("HPV_HSV_HIV_1B.csv")
data$X <- NULL

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#bac counts
data2 <-
  gather(data, key = 'Viral_Family', value = 'Counts', Herpesviridae, Papillomaviridae, 
         Retroviridae) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Family, Counts) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Family) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viral_Family']]

#Dean added to order by factor levels
vmb$Viral_Family <- factor(vmb$Viral_Family, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('purple', 'blue', 'darkgoldenrod1')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Family)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 

####
#1B2
data <- read.csv("HPV_HSV_HIV_1B2.csv")

rownames(data) <- data[,1]
data[,1] <- NULL
data <- as.data.frame(t(data))
data[is.na(data)] <- 0
data <- add_rownames(data, "Participants")

#bac counts
data2 <-
  gather(data, key = 'Viral_Family', value = 'Counts', Herpesviridae, Papillomaviridae, 
         Retroviridae) 

vmb <- tbl_df(data2) %>% # finally got the percentages correct
  group_by(Participants) %>%
  select(Participants, Viral_Family, Counts) %>%
  mutate(Group.Percentage = Counts/(sum(Counts))*100) %>% # can either have % or decimal
  arrange(Participants)

#order viral groups by abundance
abundance_order <- vmb %>% group_by(Viral_Family) %>% summarize(count = mean(Group.Percentage)) %>% arrange(desc(count)) %>% .[['Viral_Family']]

#Dean added to order by factor levels
vmb$Viral_Family <- factor(vmb$Viral_Family, 
                           levels = abundance_order)

#bar plot with custom colors
jColors <- c('purple', 'blue', 'darkgoldenrod1')

ggplot(data = vmb, aes(x = Participants, y = Group.Percentage, fill = Viral_Family)) + 
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_manual(values=jColors) +
  ylab("Relative Abundance (%)") 
