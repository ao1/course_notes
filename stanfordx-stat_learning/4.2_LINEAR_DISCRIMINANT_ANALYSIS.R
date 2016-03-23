#Set locale to USA
Sys.setlocale("LC_ALL", "C")

fp1 = file.path("C:","COURSERA","DATA SCIENCE - STANFORD - Statistical Learning")
fp2 = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - STANFORD - Statistical Learning")

if (dir.exists(fp1)) {
  default_path = fp1
} else if (dir.exists(fp2)) {
  default_path = fp2
} else {
  print('Could not set default path')
}

require(ISLR)
require(MASS)

names(Smarket)
summary(Smarket)

train = subset(Smarket,Smarket$Year < 2005)
test = subset(Smarket,Smarket$Year >= 2005)

######
######     LINEAR DISCRIMINANT ANALYSIS
######

fit = lda(Direction~Lag1+Lag2,data=train)
plot(fit)

predictions = predict(fit,newdata=test)
predictions

table(test$Direction,predictions$class)
sum(diag(table(test$Direction,predictions$class))) / sum(table(test$Direction,predictions$class))
# 55.9%

#another way to check the mean
mean(test$Direction==predictions$class)

# you can convert a list to dataframe if needed
class(predictions)
data.frame(predictions)[1:5,]





