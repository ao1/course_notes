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

eBayTrain$description = NULL
eBayTest$description = NULL

library(caTools)
spl = sample.split(eBayTrain$sold, SplitRatio = 0.7)
Train = subset(eBayTrain, spl==TRUE)
Test = subset(eBayTrain, spl==FALSE)

library(randomForest)
#install.packages("caret")
#install.packages("e1071")
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

#Random Forest
#We pick the outcome that receives the majority of the votes
#Each tree splits only on a random subset of variables
#Each tree is built from a "bagged" / "bootstrapped"  sample of data
#Bootstrapped means data is selected randomly, with replacement
#minbucket (min # of observations in subset) is called nodesize in Random Forest
#ntree (number of trees), 100-500 is usually plenty
#Outcome needs to be a factor in RandomForest, so we need to convert


Train$sold = as.factor(Train$sold)
Test$sold = as.factor(Test$sold)

Forest1 = randomForest(sold ~ biddable + startprice + condition + storage + productline, 
                       data=Train, nodesize=25 , ntree = 100)

PredictForest = predict(Forest1, newdata = Test)

sum(diag(table(Test$sold,PredictForest))) / sum(table(Test$sold,PredictForest))
#0.81


#=================================================

#cross validation with 10 folds
numFolds = trainControl(method="cv",number=10)

#define cp parameters to test seq(start,end,step)
cpGrid = expand.grid(.cp = seq(0.01,0.5,0.01))

#do cross validation, might take a while
train(sold ~ biddable + startprice + condition + storage + productline, 
      data=Train, method="rpart" , trControl=numFolds , tuneGrid=cpGrid )

CARTCV = rpart(sold ~ biddable + startprice + condition + storage + productline,
               data=Train , method = "class" , cp=0.01 )

PredictCV = predict(CARTCV,newdata = Test,type="class")

sum(diag(table(Test$sold,PredictCV))) / sum(table(Test$sold,PredictCV))
#0.79

# When we were picking different minbucket parameters before, 
# it seemed like this tree was probably not doing a good job of
# fitting the data. However, this tree with one split gives us 
# the best out-of-sample accuracy. This reminds us that sometimes 
# the simplest models are the best!

#==================================================

CART = rpart(sold ~ biddable + startprice + condition + storage + productline,
             data = Train, method="class", minbucket=25)

# Make predictions
PredictCART = predict(CART, newdata = Test, type = "class")
sum(diag(table(Test$sold, PredictCART))) / sum(table(Test$sold, PredictCART))
#0.79

#==================================================

CART2 = rpart(sold ~ biddable + startprice + condition + productline, 
                  data=Train)

CART2$variable.importance

PredictCART2 = predict(CART2, newdata = Test)
sum(diag(table(Test$sold, PredictCART2 > 0.5))) / sum(table(Test$sold, PredictCART2 > 0.5))


#force the complete tree to be built
CART3 = rpart(sold ~ biddable + startprice + condition + storage + productline,
                   data=Train, cp=0.0)


CART3$variable.importance

PredictCART3 = predict(CART3, newdata = Test)
sum(diag(table(Test$sold, PredictCART3 > 0.5))) / sum(table(Test$sold, PredictCART3 > 0.5))


#====================================================



