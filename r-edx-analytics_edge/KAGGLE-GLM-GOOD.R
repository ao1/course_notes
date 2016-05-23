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


#Logistic Regression model on train part of ebaytrain
logreg1 = glm(sold ~ biddable + startprice + condition + storage + productline , data = Train , family = "binomial")

#install.packages("car")
#collinearity reduces the acuracy of estimates
#vif=1 no collinearity, vif>5 too much colinearity, drop or combine a variable
library(car)
vif(logreg1)

#make a prediction on the test part of ebaytrain
pred_int = predict(logreg1 , newdata=Test , type = "response" )

sum(diag(table(Test$sold , pred_int  > 0.5))) / sum(table(Test$sold , pred_int > 0.5))



#delete sold variable to make ebaytrain and ebaytest have the same variables
eBayTrain$sold = NULL

#combine ebaytrain and ebaytest into a dataframe
eBayAll = rbind(eBayTrain,eBayTest)

#make external prediction on ebaytrain + ebaytest
pred_ext = predict(logreg1 , newdata=eBayAll , type = "response" )
 
MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = pred_ext)
write.csv(MySubmission, file.path(substr(default_path,0,51),"Submission1.csv"), row.names=FALSE)






