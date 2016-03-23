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
library(caret)

Train$sold = as.factor(Train$sold)
Test$sold = as.factor(Test$sold)

tr.control = trainControl(method="cv",number=10)
#Test cp values from 0.002 to 0.1 in 0.002 increments
cp.grid = expand.grid( .cp = seq(0.002,0.1,0.002))

tr = train(sold ~ biddable + startprice + condition + storage + productline, 
            data = Train, method = "rpart" , trControl = tr.control , tuneGrid = cp.grid)

#Which value of cp does the train function recommend?
attributes(tr)
tr$bestTune
#cp = 0.006

#Fit a CART model to the training data using cp = 0.006
CART1 = rpart(sold ~ biddable + startprice + condition + storage + productline,
              data=Train, method="class", cp = 0.006)

PredictCART1 = predict( CART1 , newdata = Test , type="class"  )

sum(diag(table( Test$sold , PredictCART1 ))) / sum(table( Test$sold , PredictCART1 ))

summary(CART1)
