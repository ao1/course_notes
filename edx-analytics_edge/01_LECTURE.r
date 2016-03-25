#Set locale to USA
Sys.setlocale("LC_ALL", "C")

#get current working directory
getwd()

#Don't use numbers or spaces in variable names
#Can use periods in varaible names
#R is case sensitive
#vector is like a series in pandas, dataframes are made up of vectors/series

#Variable assignation
SquareRoot2 = sqrt(2)
HoursYear <- 365*24

#show list of all variables in current session
ls()

#a vector is a list of strings or ints, but not both
#create a vector with c() function, c stands for 'combine'
Country = c("Brazil","China","India","Switzerland","USA")
LifeExp = c(74,76,65,83,79)
Country[1]

#like range() in python, seq(start,finish,step)
seq(0,100,2)

#dataframe creation from 2 vectors
CountryData = data.frame(Country,LifeExp)
#add a new column to the data frame
CountryData$Population = c(199000,139000,1240000,7997,31800)

#Make a new DataFrame
Country = c("Australia","Greece")
LifeExp = c(82,81)
Population = c(23050,11125)
NewCountryData = data.frame(Country,LifeExp,Population)

#combine dataframes
AllCountryData = rbind(CountryData,NewCountryData)
AllCountryData

#print new line
print("/n")

default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")

WHO = read.csv(file.path(default_path,"WHO.csv"))

#show dataframe structure
#gives the name, type, first few values of each variable
#Factor variable means variables have several different categories
str(WHO)
str(WHO$Under15)

#show dataframe summary.. data, groups, NAs
summary(WHO)
summary(WHO$Under15)

#create a subset dataframe
#like a boolean expression in pandas
WHO_Europe = subset(WHO , Region == 'Europe')

#write a dataframe to csv in working directory
write.csv(WHO_Europe,"WHO_Europe.csv")

ls()

#unload a dataframe from the session memory
rm(WHO_Europe)

#accessing a variable (Under15 vector)
WHO$Under15

mean(WHO$Under15)

#get standard deviation of a vector
sd(WHO$Under15)

#which row number of WHO$Under15 has the lowest Under15 population? highest?
which.min(WHO$Under15)
which.max(WHO$Under15)

#look up the Country corresponding to the 86th and 124th row
WHO$Country[86]
WHO$Country[124]

#can also combine the 2 above into
WHO$Country[which.max(WHO$Under15)]

#Which country has the smallest percentage of the population over 60?
WHO$Country[which.min(WHO$Over60)]

#Which 5 countries have the smallest percentage of the population over 60?

#Which country has the largest literacy rate?
which.max(WHO$LiteracyRate)
WHO$Country[44]

#plot(x-axis,y-axis,'p'/'l'/'b'/'c'/'o'/'h'/'s'/'S'/'n')
plot(WHO$GNI,WHO$FertilityRate)

#boolean expression to create a new dataframe of n rows
Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5)

#count number rows in dataframe
nrow(Outliers)

#show only a few variables
Outliers[c("Country","GNI","FertilityRate")]

#histogram of CellularSubscribers, to understand the distribution better
hist(WHO$CellularSubscribers)

#boxplot, to understand the statistical range of a variable
#line is the median value
#dashed lines (whiskers) show range from min to max value
boxplot(WHO$LifeExpectancy ~ WHO$Region)

#based on the results we can surmise that:
#europe has the highest media life expectancy
#the americas has the smallest inter-quartile range

#add labels to the boxplot by using xlab, ylab, main arguments
boxplot(WHO$LifeExpectancy ~ WHO$Region,xlab="",ylab="Life Expectancy", main="Life Expectancy of Countries by Region")

#summary values
table(WHO$Region)

#applies the 3rd argument to the 1st argument, groups the data by the 2nd argument
tapply(WHO$Over60,WHO$Region,mean)

#split data by region, compute minimum value of the literacy rate + remove NAs
tapply(WHO$LiteracyRate,WHO$Region,min,na.rm=TRUE)

#Which region has the lowest average child mortality rate across all countries in that region?
tapply(WHO$ChildMortality,WHO$Region,mean)
#or
which.min(tapply(WHO$ChildMortality,WHO$Region,mean))











