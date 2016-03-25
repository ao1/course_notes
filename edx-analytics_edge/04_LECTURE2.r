#Set locale to USA
Sys.setlocale("LC_ALL", "C")


if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
  
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
  
} else {
  
  print('Could not set default path')
  
}


claims = read.csv(file.path(default_path,"ClaimsData.csv"))

#percentages of amount of observations in each bucket
prop.table(table(claims$bucket2009))*100

str(claims)

library(caTools)
set.seed(88)

#split into train and test
splt = sample.split(claims$bucket2009,SplitRatio = 0.6)

train = subset(claims , splt = TRUE)
test = subset(claims , splt = FALSE)

nrow(claims)
nrow(train)
nrow(test)

#What is the average age of patients in the training set?
summary(train$age)


# What proportion of people in the training set (ClaimsTrain) 
# had at least one diagnosis code for diabetes?

unique(train$diabetes)
prop.table(table(train$diabetes))

# look at the mean in summary(train$diabetes)
# since diabetes is a binary variable the mean value of diabetes 
# gives the proportion of people with at least one diagnosis code 
# for diabetes.


#check how baseline method performs on the test set
table(test$bucket2009,test$bucket2008)


#        1      2      3      4      5
# 1 275105  19534   8641   3727    437
# 2  40001  26893  11506   7298   1401
# 3  17685  11477   6778   4173    863
# 4   6708   4778   3496   3938    923
# 5    703    491    426    778    245

#accuracy = sum of the diagonal / total number of observations

sum(diag(table(test$bucket2009,test$bucket2008)))   /  sum(table(test$bucket2009,test$bucket2008))
#accuracy = 0.6833091
#can use nrow(test) for total observations as well

#Create penalty matrix manually
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow = TRUE,nrow=5)

#convert confusion table to matrix type and multiply times PenaltyMatrix
as.matrix(table(test$bucket2009,test$bucket2008))*PenaltyMatrix

#penalty error = sum of above / total test observations
sum(as.matrix(table(test$bucket2009,test$bucket2008))*PenaltyMatrix) / nrow(test)
#0.7396207


# Suppose that instead of the baseline method discussed 
# in the previous video, we used the baseline method of 
# predicting the most frequent outcome for all observations. 
# This new baseline method would predict cost bucket 1 for everyone.

test$qq = 1
table(test$bucket2009,test$qq)
qqmatrix = matrix(c(307444,0,0,0,0,87099,0,0,0,0,40976,0,0,0,0,19843,0,0,0,0,2643,0,0,0,0),byrow = TRUE,nrow=5)
qqmatrix*PenaltyMatrix
sum(qqmatrix*PenaltyMatrix) / nrow(test) #penalty error = 1.04432
sum(diag(qqmatrix)) / nrow(test) #accuracy of baseline = 0.6712678



library(rpart)
library(rpart.plot)

#cp value was selected via cross validation on train set, big set, skipped
claimstree = rpart(bucket2009 ~ age+arthritis+alzheimers+cancer
                   +copd+depression+diabetes+heart.failure+ihd
                   +kidney+osteoporosis+bucket2008
                   +reimbursement2008, data=train,method="class",cp="0.00005")



prp(claimstree)

ls("package:rpart")

printcp(claimstree)
#93 splits
#n= 458005 


#make predictions on the test set

predicttest = predict(claimstree,newdata = test,type="class")


table(test$bucket2009,predicttest)

#        1      2      3      4      5
# 1 287072  20124      0    248      0
# 2  46188  40662      0    249      0
# 3  20376  20421      0    179      0
# 4   7934  11480      0    429      0
# 5    854   1710      0     79      0

#accuracy = 0.7165053
sum(diag(table(test$bucket2009,predicttest))) / nrow(test)

#penalty error = 0.7577232
sum( as.matrix(table(test$bucket2009,predicttest)) * PenaltyMatrix  ) / nrow(test)

#accuracy and penalty error both went up, CART model predicts 3,4,5 rarely
#this model wont do better on the penalty error


#rpart allows us to specify penalty matrix with the parms argument
  
claimstree2 = rpart(bucket2009 ~ age+arthritis+alzheimers+cancer
                   +copd+depression+diabetes+heart.failure+ihd
                   +kidney+osteoporosis+bucket2008
                   +reimbursement2008, data=train , method="class" , cp="0.00005" , parms=list(loss=PenaltyMatrix))

  
predicttest2 = predict(claimstree2,newdata = test,type="class")
  
table(test$bucket2009,predicttest2)
  
#        1      2      3      4      5
# 1 233636  66713   6518    577      0
# 2  17191  50846  17683   1379      0
# 3   8675  20682  10761    858      0
# 4   3176   8375   6900   1392      0
# 5    334    882   1062    365      0

sum(diag(table(test$bucket2009,predicttest2))) / nrow(test)
sum( as.matrix(table(test$bucket2009,predicttest2)) * PenaltyMatrix  ) / nrow(test)

#accuracy = 0.6476676
#penalty error = 0.6386895

#accuracy lower than baseline, but penalty error is lower too
#if we had more independent variables, we'd do even better with this CART model

#Did the second CART model, with the loss matrix, 
#predict bucket 1 for more or fewer of the observations?

362424/sum(summary(predicttest))  #0.7913101

263012/sum(summary(predicttest2))  #0.5742557





