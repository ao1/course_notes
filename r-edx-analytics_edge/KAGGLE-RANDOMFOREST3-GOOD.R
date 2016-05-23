Sys.setlocale("LC_ALL", "C")

if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
} else {
  next
}

eBayTrain = read.csv(file.path(default_path,"eBayiPadTrain.csv"))
eBayTest = read.csv(file.path(default_path,"eBayiPadTest.csv"))

eBayTrain$description = NULL
eBayTest$description = NULL

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

#generate a random forest model with all of the variables
RF = randomForest(sold ~ biddable + startprice + condition + storage + productline ,
                  data=Train)

# Remember that you don't need a "type" argument when making 
# predictions with a random forest model if you want to use 
# a threshold of 0.5

PredictRF = predict(RF,newdata = Test)

sum(diag(table(Test$sold,PredictRF))) / sum(table(Test$sold,PredictRF))
#accuracy = 0.8348839

# levels(eBayTest$biddable) = levels(eBayTrain$biddable)
# levels(eBayTest$startprice) = levels(eBayTrain$startprice)
# levels(eBayTest$condition) = levels(eBayTrain$condition)
# levels(eBayTest$cellular) = levels(eBayTrain$cellular)
# levels(eBayTest$carrier) = levels(eBayTrain$carrier)
# levels(eBayTest$color) = levels(eBayTrain$color)
# levels(eBayTest$storage) = levels(eBayTrain$storage)
levels(eBayTest$productline) = levels(eBayTrain$productline)

PredictSubmission = predict(RF,newdata = eBayTest)

# One metric that we can look at is the number of times, 
# aggregated over all of the trees in the random forest model, 
# that a certain variable is selected for a split

# This code produces a chart that for each variable measures 
# the number of times that variable was selected for splitting 
# (the value on the x-axis)

#vu = varUsed(RF, count=TRUE)

#vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

#dotchart(vusorted$x, names(RF$forest$xlevels[vusorted$ix]))

MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = PredictSubmission)
write.csv(MySubmission, file.path(substr(default_path,0,51),"Submission2.csv"), row.names=FALSE)


