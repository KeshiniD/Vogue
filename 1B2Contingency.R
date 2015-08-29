#2x2contingency table
a <- xtabs(~Nugent.score + Ethnicity , data = total)
kable(a)
#3x3 table
a <- xtabs(~Nugent.score + Ethnicity + Marital.Status , data = total)
a <- as.data.frame(a)

#Contingency Tables
attach(total)
#can just give names, since used attach()
mytable <- table(total$Ethnicity.cat,total$BMI.cat) #has to be same lengths
mytable

attach(ExampleData) # seems to work only with 0,1 categories
mytable <- table(Age, Sex)
mytable

attach(data) #works 
mytable <- table(abnormal.discharge..y.1..n.0., Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0.) 
mytable
margin.table(mytable, 1) # A frequencies (summed over B) AD over douche
margin.table(mytable, 2) # B frequencies (summed over A) douche over AD
prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) #column percentages

# for 3-way table
mytable <- table(Abnormal.discharge.2wks, 
                 Use.of.feminine.wipes.or.genital.deodrant..y.1..n.0., 
                 Genital.Infections..y.1..n.0.) 
ftable(mytable)
summary(mytable) #chi-squared
#table() ignored NA values, if want use exclude=NULL and works

#Fisher's and Chisquared
#can only use both for 2x2 matrix
chisq.test(mytable)
fisher.test(mytable) #gives odds ratio
mantelhaen.test(mytable) #only use when counts greater than 1 in each cell
