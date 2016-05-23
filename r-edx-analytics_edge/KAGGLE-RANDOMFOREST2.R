Sys.setlocale("LC_ALL", "C")

if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
} else {
  next
}

# Add the argument stringsAsFactors=FALSE since we have some text fields
eBayTrain = read.csv(file.path(default_path,"eBayiPadTrain.csv"), stringsAsFactors=TRUE)
eBayTest = read.csv(file.path(default_path,"eBayiPadTest.csv"), stringsAsFactors=TRUE)

library(caTools)
spl = sample.split(eBayTrain$sold, SplitRatio = 0.7)
Train = subset(eBayTrain, spl==TRUE)
Test = subset(eBayTrain, spl==FALSE)

library(randomForest)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

Train$sold = as.factor(Train$sold)
Test$sold = as.factor(Test$sold)

Forest1 = randomForest(sold ~ biddable + startprice + condition + storage + productline, 
                       data=Train, nodesize=25 , ntree = 100)

summary(Forest1)

PredictForest1 = predict(Forest1, newdata = Test)

sum(diag(table(Test$sold,PredictForest1))) / sum(table(Test$sold,PredictForest1))


# levels(eBayTest$description) = levels(eBayTrain$description)
# levels(eBayTest$biddable) = levels(eBayTrain$biddable)
# levels(eBayTest$startprice) = levels(eBayTrain$startprice)
# levels(eBayTest$condition) = levels(eBayTrain$condition)
# levels(eBayTest$cellular) = levels(eBayTrain$cellular)
# levels(eBayTest$carrier) = levels(eBayTrain$carrier)
# levels(eBayTest$color) = levels(eBayTrain$color)
# levels(eBayTest$storage) = levels(eBayTrain$storage)
# levels(eBayTest$productline) = levels(eBayTrain$productline)
# 
# 
# PredictSubmission = predict(Forest1, newdata = eBayTest)
# MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = PredictSubmission)
# write.csv(MySubmission, file.path(substr(default_path,0,51),"Submission2.csv"), row.names=FALSE)








