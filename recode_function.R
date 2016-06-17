recoderFunc <- function(data, oldvalue, newvalue) {
  
  # convert any factors to characters
  
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  
  # create the return vector
  
  newvec <- data
  
  # put recoded values into the correct position in the return vector
  
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  
  newvec
  
}

recoderFunc(data, oldvalue, newvalue)
#set stringsAsFactors=FALSE when readin files, if function does not work

#read virla reference manual
vref <- read.csv(file="virus_ref.csv", stringsAsFactors=FALSE, header=FALSE)
#omit empty rows and columns
vref <- vref %>%
  select(V1, V2)
vref <- vref[c(1:105),]

#load above function
#load viral DNA, RNA and Phage family data
vDNA <- read.csv(file="Viral_family_DNA.csv", stringsAsFactors=FALSE)
vRNA <- read.csv(file="Viral_family_RNA.csv", stringsAsFactors = FALSE)
phage <- read.csv(file="phage_family_DNA.csv", stringsAsFactors = FALSE)

#omit empty column
vDNA$X <- NULL
vRNA$X <- NULL
phage$X <- NULL

#recode
vDNA2 <- recoderFunc(vDNA, vref$V1, vref$V2)
vRNA2 <- recoderFunc(vRNA, vref$V1, vref$V2)
phage2 <- recoderFunc(phage, vref$V1, vref$V2)

#write to file
# write.csv(vDNA2, "Viral_family_DNA_postref.csv")
# write.csv(vRNA2, "Viral_family_RNA_postref.csv")
# write.csv(phage2, "phage_family_DNA_postref.csv")
#calculate freq of counts and call back

#load data
vDNA <- read.csv(file="Viral_family_DNA_postref.csv", header=TRUE)
vRNA <- read.csv(file="Viral_family_RNA_postref.csv", header=TRUE)
phage <- read.csv(file="phage_family_DNA_postref.csv", header=TRUE)

#omit empty column
vDNA$X <- NULL
vRNA$X <- NULL
phage$X <- NULL

#gathering like species                    
vDNA2 <- ddply(vDNA,c("Var1"),numcolwise(sum)) #includes all columns
vRNA2 <- ddply(vRNA,c("Var1"),numcolwise(sum)) #includes all columns
phage2 <- ddply(phage,c("Var1"),numcolwise(sum)) #includes all columns

#write these three to file
# write.csv(vDNA2, "Viral_family_DNA_consolidated.csv")
# write.csv(vRNA2, "Viral_family_RNA_consolidated.csv")
# write.csv(phage2, "phage_family_DNA_consolidated.csv")

