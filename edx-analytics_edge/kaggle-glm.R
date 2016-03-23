Sys.setlocale("LC_ALL", "C")

if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
} else {
  next
}

# Add the argument stringsAsFactors=FALSE since we have some text fields
eBayTrain = read.csv(file.path(default_path,"eBayiPadTrain.csv"), stringsAsFactors=FALSE)
eBayTest = read.csv(file.path(default_path,"eBayiPadTest.csv"), stringsAsFactors=FALSE)

#  description: chr  
#  biddable   : int  
#  startprice : num
#  condition  : chr
#  cellular   : chr
#  carrier    : chr
#  color      : chr
#  storage    : chr
#  productline: chr
#  sold       : int
#  UniqueID   : int

eBayTrain$description = NULL
eBayTest$description = NULL

#Logistic Regression
logreg1 = glm(sold ~ . -UniqueID -color -carrier, data = eBayTrain , family = "binomial")

#install.packages("car")
#collinearity reduces the acuracy of estimates
#vif=1 no collinearity, vif>5 too much colinearity, drop or combine a variable
library(car)
vif(logreg1)

pred_test_logreg1 = predict(logreg1 , newdata=eBayTest , type = "response" )
pred_train_logreg1 = predict(logreg1 , newdata=eBayTrain , type = "response" )
 
sum(diag(table(eBayTrain$sold , pred_train_logreg1 > 0.5))) / sum(table(eBayTrain$sold , pred_train_logreg1 > 0.5))
#0.8049436



MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = pred_test_logreg1)
write.csv(MySubmission, file.path(substr(default_path,0,51),"Submission1.csv"), row.names=FALSE)









