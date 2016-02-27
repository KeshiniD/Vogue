data <- read.csv(file = "1B2metabac_condensedv2.csv")
summary(lm(data$Shannon.s.Diversity~data$BV..number.of.episodes.year.))
cor.test(data$Shannon.s.Diversity,data$BV..number.of.episodes.year.)
# sim p-value

t.test(data$Shannon.s.Diversity~data$Age.cat)
#all good :)