#Set locale to USA
Sys.setlocale("LC_ALL", "C")


if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
  
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
  
} else {
  
  print('Could not set default path')
  
}

stevens = read.csv(file.path(default_path,"stevens.csv"))


str(stevens)

# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)

# Install rpart library
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)

# CART model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=25)

prp(StevensTree)

# Make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71) # accuracy 0.659, 

#baseline that always predicts reverse accuracy 0.547

# ROC curve
library(ROCR)

PredictROC = predict(StevensTree, newdata = Test)

PredictROC  #shows probabtility of outcome 0 and 1 for each observation in test set

#use the 2nd column (prob of 1) to generate an ROC curve

#prediction(probability of 1 result , true outcome values)

pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")  #tpr = true positive rate
plot(perf)


#Compute the AUC of the CART model
as.numeric(performance(pred, "auc")@y.values)
#0.6927105


#build a CART model with the minbucket parameter to 5. 
StevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=5)

#Plot the new tree.
prp(StevensTree)

#Show all functions of rpart
ls("package:rpart")
# 
# [1] "car.test.frame" "car90"          "cu.summary"     "kyphosis"      
# [5] "meanvar"        "na.rpart"       "path.rpart"     "plotcp"        
# [9] "post"           "printcp"        "prune"          "prune.rpart"   
# [13] "rpart"          "rpart.control"  "rpart.exp"      "rsq.rpart"     
# [17] "snip.rpart"     "solder"         "stagec"         "xpred.rpart"  

#print model summary
printcp(StevensTree2)

#Max n split value is 16 (number of splits)
#This tree is probably overfit to the training data, and is not as interpretable.


#build a CART model with the minbucket parameter to 100. 
StevensTree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=100)

prp(StevensTree3)

printcp(StevensTree3)

#The tree only has 1 split! 
#This tree is probably not fit well enough to the training data.



#install.packages("randomForest")
library(randomForest)

#Random Forest

#More computationally intensive
#Designed to improve CART prediction accuracy
#Works on a large amount of tress
#Makes the model less interpretable
#Each tree in forest votes on the outcome
#We pick the outcome that receives the majority of the votes
#Each tree splits only on a random subset of variables
#Each tree is built from a "bagged" / "bootstrapped"  sample of data
#Bootstrapped means data is selected randomly, with replacement
#minbucket (min # of observations in subset) is called nodesize in Random Forest
#ntree (number of trees), 100-500 is usually plenty


#Outcome needs to be a factor in RandomForest, so we need to convert
#Reverse to a factor variable

Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, nodesize=25 , ntree = 200)

#compute predictions on test set
PredictForest = predict(StevensForest, newdata = Test)

#look up the confusion matrix to check accuracy (it might be random)
table(Test$Reverse,PredictForest)

#Model Accuracy:  0.6647059   = improved accuracy by 1%
#Baseline Accuracy:  0.4529412 

#Sometimes RF can significantly improve accuracy over CART, sometimes not much.


#set the seed to 200, and the re-build the random forest model
set.seed(100)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + 
Respondent + LowerCourt + Unconst, data=Train, nodesize=25 , ntree = 200)
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse,PredictForest)

set.seed(200)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + 
Respondent + LowerCourt + Unconst, data=Train, nodesize=25 , ntree = 200)
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse,PredictForest)


# As we see here, the random component of the random forest method 
# can change the accuracy. The accuracy for a more stable dataset will 
# not change very much, but a noisy dataset can be significantly 
# affected by the random samples.


#install.packages("caret")
#install.packages("e1071")

library(caret)
library(e1071)

#cross validation with 10 folds
numFolds = trainControl(method="cv",number=10)

#define cp parameters to test seq(start,end,step)
cpGrid = expand.grid(.cp = seq(0.01,0.5,0.01))

#do cross validation, might take a while
train(Reverse ~ Circuit + Issue + Petitioner + 
                Respondent + LowerCourt + Unconst, 
      data=Train, method="rpart" , trControl=numFolds , tuneGrid=cpGrid )


# columns:
# cp parameter tested / cross validation accuracy / 

#The final value used for the model was cp = 0.19. 





StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + 
                      Respondent + LowerCourt + Unconst , data=Train , method = "class" , cp=0.18 )


PredictCV = predict(StevensTreeCV,newdata = Test,type="class")


table(Test$Reverse,PredictCV)
#
# PredictCV
# 0  1
# 0 59 18
# 1 29 64

#Accuracy: 59+64/All vars ~ 0.724

#plot
prp(StevensTreeCV)
#summary
printcp(StevensTreeCV)

# When we were picking different minbucket parameters before, 
# it seemed like this tree was probably not doing a good job of
# fitting the data. However, this tree with one split gives us 
# the best out-of-sample accuracy. This reminds us that sometimes 
# the simplest models are the best!













