#Set locale to USA
Sys.setlocale("LC_ALL", "C")

default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")

USDA = read.csv(file.path(default_path,"USDA.csv"))

#show basic info about data frame
str(USDA)
summary(USDA)

#find all names that have 'caviar' in USDA$Description
grep('caviar',USDA$Description,ignore.case=TRUE)
match("CAVIAR",USDA$Description)

#Lookup caviar sodium number  
USDA$Sodium[match("CAVIAR",USDA$Description)]

#which row has the maximum amount of sodium?
USDA$Description[which.max(USDA$Sodium)]

#show names of all varaibles in data frame
names(USDA)

#make a subset of the original dataframe with only rows where Sodium>10000
HighSodium = subset(USDA,Sodium>10000)

#show number of rows in new dataframe
nrow(HighSodium)

#show names of all 
HighSodium$Description

#summary of only the sodium vector
summary(USDA$Sodium)

#sodium standard deviation
sd(USDA$Sodium,na.rm = TRUE)

#plot Protein count vs TotalFat, plot(x,y)
plot(USDA$Protein,USDA$TotalFat,xlab = "Protein",ylab = "Fat",main = "Protein vs Fat", col = "brown")

#histogram
hist(USDA$VitaminC,xlab="Vitamin C (mg)",main="Histogram of Vitamin C Levels")

#limit x axis to between 1 and 100
hist(USDA$VitaminC,xlim=c(1,100),xlab="Vitamin C (mg)",main="Histogram of Vitamin C Levels")

#break up 1 cell into smaller pieces by using breaks arg
hist(USDA$VitaminC,xlim=c(1,100),breaks=2000,xlab="Vitamin C (mg)",main="Histogram of Vitamin C Levels")

#boxplot
boxplot(USDA$Sugar,ylab = "Sugar in Grams",main="Boxplot of Sugar Levels")


#add new variable to USDA dataframe that has a 1 if Sodium>avg and 0 is Sodium<avg
USDA$Sodium[1] > mean(USDA$Sodium,na.rm = TRUE)
HighSodium = USDA$Sodium > mean(USDA$Sodium,na.rm = TRUE)
str(HighSodium)
#convert vector from boolean to numeric
HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium,na.rm = TRUE))

#add these new variables to dataframe
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium,na.rm = TRUE))
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein,na.rm = TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat,na.rm = TRUE))
USDA$HighCarbohydrate = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate,na.rm = TRUE))

#how many entires have high sodium? (1 above avg, 0 below avg)
table(USDA$HighSodium)

#how many entires have high sodium and high fat? any other combination? 
table(USDA$HighSodium,USDA$HighFat)
#712 foods have both high sodium and high fat (two 1s)

#applies the 3rd argument to the 1st argument, sort the data by the 2nd argument
#get the average level of Iron, sorted by High Protein

tapply(USDA$Iron,USDA$HighProtein,mean,na.rm=TRUE)

#foods with low protein content have on avg 2.55 mg of iron
#foods with high protein content have on avg 3.19 mg of iron

#Maximum level of vitamin C in foods with high and low carbs
tapply(USDA$VitaminC,USDA$HighCarbohydrate,max,na.rm=TRUE)
tapply(USDA$VitaminC,USDA$HighCarbohydrate,summary,na.rm=TRUE)
#it seems that foods with high carb content have more vitamin C






