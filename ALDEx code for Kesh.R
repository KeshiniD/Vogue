####################
# aldex analyses of 1A samples
#####################

library(ALDEx2m)

######################
# load data

setwd("~/WHRI/VOGUE/1A/analysis Sept 2014")

# this is a file of the read counts with samples as columns and nearest neighbours as rows
v1a.nn <- read.csv("Vogue1A_labels_updated_frequency_table_22Sept2014.csv", header = TRUE)
head(v1a.nn)

# set the rownames as the taxa names
row.names(v1a.nn) <- v1a.nn[, 1]
v1a.nn <- v1a.nn[, -1]

# metadata with rows as samples and columns as variables. The rows must be in the same order as the columns of the read count file.
v1a.meta <- read.csv("vogue1A_metadata_CST_17SEPT2014_final.csv", header = TRUE)
head(v1a.meta)

########
# ALDEx can be run on any number of categories of your metatdata

# Highest.Education.Level.Attained
table(v1a.meta$Highest.Education.Level.Attained)
Graduate Degree  Post-secondary/Undergraduate Degree some High School/High School diploma 
60                                  153                                   97

# make a vector that is the variable labels
cond.edu <- v1a.meta$Highest.Education.Level.Attained

# run ALDEx
ald.edu <- aldex(reads = v1a.nn, conditions = cond.edu, test = "glm", effect = FALSE)

# look at the output
head(ald.edu)

# smallest p-values
min(ald.edu$glm.eBH) # 0.7229061
min(ald.edu$kw.eBH) # 0.7753618