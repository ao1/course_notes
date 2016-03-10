source("http://www.openintro.org/stat/data/cdc.R")

names(cdc)
str(cdc)
summary(cdc)

mean(cdc$weight) 
var(cdc$weight)
median(cdc$weight)

#relative frequency distribution
table(cdc$smoke100)/20000

#basic barplot of the smoke variable
barplot(table(cdc$smoke100))

#how many males are in the dataset?
table(cdc$gender)

#What proportion of the dataset reports being in excellent health?
prop.table(table(cdc$genhlth))

mosaicplot(table(cdc$gender,cdc$smoke100))

#show sixth variable of the 567th respondent, 567th row, 6th column
cdc[567,6]

#show weights for the first 10 respondents
cdc[1:10,6]

#all data for first 10 respondents
cdc[1:10,]

#weight for the 567th respondent 
cdc$weight[567]

#create a few subsets from cdc
mdata = subset(cdc, cdc$gender == "m")
m_and_over30 = subset(cdc, cdc$gender == "m" & cdc$age > 30)
m_or_over30 = subset(cdc, cdc$gender == "m" | cdc$age > 30)
under23_and_smoke = subset(cdc, cdc$smoke100 == 1 & cdc$age < 23)


#The ~ character can be read "versus" or "as a function of". 
#So we're asking R to give us a box plots of heights where the 
#groups are defined by gender.
boxplot(cdc$height ~ cdc$gender)

#add a bmi variable
cdc$bmi = (cdc$weight / cdc$height^2) * 703
#box plot defining groups by the variable cdc$genhlth.
boxplot( cdc$bmi ~ cdc$genhlth )

#histograms
hist(cdc$age)
hist(cdc$bmi)
hist(cdc$bmi, breaks = 50)

#make a scatterplot of weight versus desired weight.
plot(cdc$weight , cdc$wtdesire)

