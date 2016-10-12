#bacterial diversity for 1A, 1B, 1B2

#load data
# vogueA <- read.delim("Vogue1A_microbiomedata.txt")
vogueA <- read.csv("1A_1B2_diversitystats.csv")
vogueB <- read.csv("Vogue1B_bac.csv")
vogue1B2 <- read.csv("1B2metabac_condensedv2.csv")

#select participants in virome
#1B2
vogue1B2 <- vogue1B2 %>%
  select(Participants, Shannon.s.Diversity)
vogue1B2a <- vogue1B2[which(vogue1B2$Participants == "Vogue1B2.01.06" | vogue1B2$Participants == "Vogue1B2.01.07" | vogue1B2$Participants == "Vogue1B2.01.08" | vogue1B2$Participants == "Vogue1B2.01.09" | vogue1B2$Participants == "Vogue1B2.01.10" | vogue1B2$Participants == "Vogue1B2.01.11" | vogue1B2$Participants == "Vogue1B2.01.12" | vogue1B2$Participants == "Vogue1B2.01.15"),]

#1A
vogueA <- vogueA %>%
  select(Participants, Shannon.s.Diversity)
vogueA2 <- vogueA[c(50,55,57,58,60,61,64:67,70:74,77,80,81,88,
                    97,102),]

#1B
vogueB2 <- vogueB[c(1,3,5:8,10,11,13:15,17,19,23,28,29,34,36,38:40,42,45,50,53,54)]

#move species into headers, and participants into rows
rownames(vogueB2) <- vogueB2[,1]
vogueB2[,1] <- NULL
vogueB2 <- as.data.frame(t(vogueB2))

#Shannon Diversity
#just have the species
H <- diversity(vogueB2) #shannon for individuals
H <- data.frame(H)
View(H)

#rename headers
H <- dplyr::rename(H, Shannon.s.Diversity = H)
H <- add_rownames(H, "Participants")

#write to file
# write.csv(H, "bac_SD_1B.csv")

#join all bacterial diversity together
total <- join(vogueA2, H, type="full")
total <- join(total, vogue1B2a, type="full")

#write to file
# write.csv(total, "virome_bac_SD_all.csv")
