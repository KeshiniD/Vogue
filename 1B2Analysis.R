nwdata <- read.delim(file.path("1B2_26_64.txt"))
olddata <- read.delim(file.path ("1B2data_part1.txt"))
olddata  <- olddata %>%
  select (Simple.name, Group, Total.Frequency, Vogue1B2.01.01, Vogue1B2.01.06, 
          Vogue1B2.01.07, Vogue1B2.01.08, Vogue1B2.01.09, Vogue1B2.01.10, 
          Vogue1B2.01.11, Vogue1B2.01.12, Vogue1B2.01.15, Vogue1B2.01.19, 
          Vogue1B2.01.21, Vogue1B2.01.23, Vogue1B2.01.28, Vogue1B2.01.29)
                      
data2 <- aggregate(TOTAL.frequency~Simple.name,nwdata,FUN=sum) #only total frew

data2 <- ddply(nwdata,"Simple.name",numcolwise(sum)) #everything

#write to file
write.table(data2, "new1B2.csv", sep = ",", row.names = FALSE, quote = FALSE)
write.table(nwdata, "1B2_26_64.csv", sep = ",", row.names = FALSE, quote = FALSE)
write.table(olddata, "1_291B2.csv", sep = ",", row.names = FALSE, quote = FALSE)
  
#all bacteria
         Actinobacteria sp., 
         Actinomyces neuii subsp. anitratus,Alloscardovia omnicolens, 
         Atopobium vaginae, Bifidobacterium breve, Eggerthella lenta,
         Gardnerella vaginalis Group A, Gardnerella vaginalis Group C, 
         Gardnerella vaginalis Group D, Paraprevotella clara, 
         Prevotella amnii, Prevotella buccalis, Prevotella dentalis, 
         Prevotella dentasini, Prevotella disiens, Prevotella loescheii, 
         Prevotella melaninogenica, Prevotella oralis, 
         Prevotella paludivivens, Prevotella tannerae, 
         Prevotella timonensis, 
         Prevotella veroralis, Bacteroides pectinophilus, 
         Porphyromonas uenonis, Prevotella bivia, 
         Prosthecochloris vibrioformis, Blautia hansenii, 
         Megasphaera sp. genomosp. type_1, Aerococcus urinae, 
         Atopostipes suicloacalis, Lactobacillus acidophilus, 
         Lactobacillus coleohominis, Lactobacillus crispatus, 
         Lactobacillus gasseri, Lactobacillus iners, 
         Lactobacillus jensenii, Lactobacillus plantarum, 
         Lactobacillus reuteri, Lactobacillus ultunensis, 
         Leuconostoc lactis, Streptococcus devriesei, 
         Streptococcus infantis, Streptococcus mitis, 
         Streptococcus pseudopneumoniae, 
         Streptococcus salivarius subsp. salivarius, 
         Streptococcus sanguinis, Streptococcus suis, 
         Streptococcus urinalis, Streptococcus vestibularis, 
         Streptococcus waius, Caldicellulosiruptor saccharolyticus, 
         Clostridia sp. BVAB2, Clostridium genomosp. BVAB3, 
         Clostridium stercorarium subsp. stercorarium, 
         Desulfotomaculum hydrothermale, Eubacterium coprostanoligenes, 
         Eubacterium eligen, Faecalibacterium cf. prausnitzii, 
         Peptoniphilus harei, Peptoniphilus lacrimalis, 
         Veillonella parvula, Coprococcus eutactus, Bulleidia extructa, 
         Acidaminococcus fermentans, Anaeroglobus geminatus, 
         Dialister invisus, Megasphaera cerevisiae, Mitsuokella multacida, 
         Streptobacillus moniliformis, Trichechus manatus latirostris, 
         Prevotella bergensis, Acinetobacter junii, Acinetobacter lwoffii, 
         Burkholderia glumae, Escherichia coli, Haemophilus haemolyticus, 
         Magnetospirillum magnetotacticum, Methylobacillus flagellatus, 
         Pseudomonas fluorescens, Ralstonia pickettii, 
         Serratia proteamaculans, Shigella sonnei)