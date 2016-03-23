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
require(class) #knn in here

######
######     CREATE TRAIN AND TEST SETS
######

names(Smarket)
summary(Smarket)
# subset train and test sets
train = subset(Smarket,Smarket$Year < 2005)
test = subset(Smarket,Smarket$Year >= 2005)

# bind Lag1 and Lag2 into one object
xlag_train = cbind(train$Lag1,train$Lag2)
xlag_test = cbind(test$Lag1,test$Lag2)

######
######     K NEAREST NEIGHBORS
######

?knn

predictions = knn(xlag_train,xlag_test,train$Direction,k=1)

# see how we did
table(predictions,test$Direction)
sum(diag(table(predictions,test$Direction))) / sum(table(predictions,test$Direction))
# 50% so 1 nearest neighbor did no better than flippig a coin 
# we can try higher values of k

#another method for showing the above
mean(predictions==test$Direction)


# try with k=100
predictions2 = knn(xlag_train,xlag_test,train$Direction,k=100)
mean(predictions2==test$Direction)
#54% better
