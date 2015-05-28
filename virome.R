#load packages
library(plyr)
library(dplyr)
library(tidyr)

#load data
data <- read.delim(file.path("table.tsv"))

# subset of data and totals
vmb <- tbl_df(data) %>% 
  select(domain, abundance) %>%
  group_by(domain) %>%
  summarize(TotalCounts = sum(abundance)) %>%
  arrange(domain)

#write to file
write.table(vmb, "virome_ratios.tsv", sep = ",", row.names = FALSE, 
            quote = FALSE)

