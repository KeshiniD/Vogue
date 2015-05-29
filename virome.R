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

#ratio column
min <- min(vmb$TotalCounts)
vmb2 <- tbl_df(vmb) %>% 
  group_by(domain) %>%
  summarize(Ratio = TotalCounts/min) %>% 
  arrange(domain)

#write to file
write.table(vmb2, "virome_ratios.tsv", sep = "\t", row.names = FALSE, 
            quote = FALSE)


