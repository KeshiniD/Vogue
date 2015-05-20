data <- read.delim(file.path("VOGUE_1B_HIV_data.txt"))
library(plyr)
##suppresses start up messages
suppressPackageStartupMessages(library(dplyr)) 
library(ggplot2)
library(tidyr)
library(knitr)
library(assertthat)

vmb <- tbl_df(data) %>% # collection vs HIV
  arrange(Event.Name) %>%

myData = vmb[-c(1:94), ]

write.table(myData, "myData.csv", sep = ",", row.names = FALSE, 
            quote = FALSE)