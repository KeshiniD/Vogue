#called recent data participants 26 to 64
nwdata <- read.delim(file.path("1B2_26_64.txt"))

#called older data participants 01 to 29 (26 in newer data)
olddata <- read.delim(file.path ("1B2data_part1.txt"))

#ordering participant data
olddata  <- olddata %>%
  select (Simple.name, Group, Total.Frequency, Vogue1B2.01.01, Vogue1B2.01.06, 
          Vogue1B2.01.07, Vogue1B2.01.08, Vogue1B2.01.09, Vogue1B2.01.10, 
          Vogue1B2.01.11, Vogue1B2.01.12, Vogue1B2.01.15, Vogue1B2.01.19, 
          Vogue1B2.01.21, Vogue1B2.01.23, Vogue1B2.01.28, Vogue1B2.01.29)

#gathering like bacterial species                    
data2a <-aggregate(TOTAL.frequency~Simple.name,nwdata,FUN=sum) #only includes total freq
data2 <- ddply(nwdata,c("Simple.name", "Group"),numcolwise(sum)) #all columns

#write to file
write.table(data2, "new1B2.csv", sep = ",", row.names = FALSE, quote = FALSE)
write.table(nwdata, "1B2_26_64.csv", sep = ",", row.names = FALSE, quote = FALSE)
write.table(olddata, "1_291B2.csv", sep = ",", row.names = FALSE, quote = FALSE)
  
#list of all bacterial species in both datasets
         #Actinobacteria sp., 
         #Actinomyces neuii subsp. anitratus,Alloscardovia omnicolens, 
         #Atopobium vaginae, Bifidobacterium breve, Eggerthella lenta,
         #Gardnerella vaginalis Group A, Gardnerella vaginalis Group C, 
         #Gardnerella vaginalis Group D, Paraprevotella clara, 
         #Prevotella amnii, Prevotella buccalis, Prevotella dentalis, 
         #Prevotella dentasini, Prevotella disiens, Prevotella loescheii, 
         #Prevotella melaninogenica, Prevotella oralis, 
         #Prevotella paludivivens, Prevotella tannerae, 
         #Prevotella timonensis, 
         #Prevotella veroralis, Bacteroides pectinophilus, 
         #Porphyromonas uenonis, Prevotella bivia, 
         #Prosthecochloris vibrioformis, Blautia hansenii, 
         #Megasphaera sp. genomosp. type_1, Aerococcus urinae, 
         #Atopostipes suicloacalis, Lactobacillus acidophilus, 
         #Lactobacillus coleohominis, Lactobacillus crispatus, 
         #Lactobacillus gasseri, Lactobacillus iners, 
         #Lactobacillus jensenii, Lactobacillus plantarum, 
         #Lactobacillus reuteri, Lactobacillus ultunensis, 
         #Leuconostoc lactis, Streptococcus devriesei, 
         #Streptococcus infantis, Streptococcus mitis, 
         #Streptococcus pseudopneumoniae, 
         #Streptococcus salivarius subsp. salivarius, 
         #Streptococcus sanguinis, Streptococcus suis, 
         #Streptococcus urinalis, Streptococcus vestibularis, 
         #Streptococcus waius, Caldicellulosiruptor saccharolyticus, 
         #Clostridia sp. BVAB2, Clostridium genomosp. BVAB3, 
         #Clostridium stercorarium subsp. stercorarium, 
         #Desulfotomaculum hydrothermale, Eubacterium coprostanoligenes, 
         #Eubacterium eligen, Faecalibacterium cf. prausnitzii, 
         #Peptoniphilus harei, Peptoniphilus lacrimalis, 
         #Veillonella parvula, Coprococcus eutactus, Bulleidia extructa, 
         #Acidaminococcus fermentans, Anaeroglobus geminatus, 
         #Dialister invisus, Megasphaera cerevisiae, Mitsuokella multacida, 
         #Streptobacillus moniliformis, Trichechus manatus latirostris, 
         #Prevotella bergensis, Acinetobacter junii, Acinetobacter lwoffii, 
         #Burkholderia glumae, Escherichia coli, Haemophilus haemolyticus, 
         #Magnetospirillum magnetotacticum, Methylobacillus flagellatus, 
         #Pseudomonas fluorescens, Ralstonia pickettii, 
         #Serratia proteamaculans, Shigella sonnei)

#merge olddata and data2 (1B2-01 to -29 and -26 to -64)
#need to rename column so that it is the same in both data sets
#will now merge based on this column
data2 <- dplyr::rename(data2, Total.Frequency = TOTAL.frequency)
#not what I want, see bottom code
total <- merge(data2, olddata, by="Simple.name", all = TRUE) 

#this one merges the data frame 
zz<-join(data2, olddata, type="full")
zz[is.na(zz)] <- 0

#aggregates the like observations from merged data frame
dataall <- ddply(zz,c("Simple.name", "Group"),numcolwise(sum)) #everything

#write this data to file
write.table(dataall, "complete1B2data.csv", sep = ",", row.names = FALSE, quote = FALSE)

#assigned bacterial groups and called back
data <- read.csv(file.path("complete1B2data.csv"))
#grouped based on bacteria labels
cdata <- ddply(data,"Group",numcolwise(sum))

#write this table to file
write.table(cdata, "1B2data_groups.csv", sep = ",", row.names = FALSE, quote = FALSE)
