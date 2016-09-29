#plot
#call data
everyone <- read.csv("HPV_HSV_HIV_all.csv")
vogueA <- read.csv("HPV_HSV_HIV_1A.csv")
vogueB <- read.csv("HPV_HSV_HIV_1B.csv")
vogue1b2 <- read.csv("HPV_HSV_HIV_1B2.csv")

#to order alphabetically
a <- a[order(a$Viral_Species),]

