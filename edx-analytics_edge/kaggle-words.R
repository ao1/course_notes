#Set locale to USA
Sys.setlocale("LC_ALL", "C")

if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
} else {
  next
}

# KAGGLE COMPETITION - DEALING WITH THE TEXT DATA

# Read in the data into R, adding in the argument stringsAsFactors=FALSE, since we have some text fields.

eBayTrain = read.csv(file.path(default_path,"eBayiPadTrain.csv"), stringsAsFactors=FALSE)
eBayTest = read.csv(file.path(default_path,"eBayiPadTest.csv"), stringsAsFactors=FALSE)

#install.packages("SnowballC")

library(tm)
library(SnowballC)

# You can use other variables in the dataset for text analytics, but we will 
# just show you how to use this particular variable. 
# Note that we are creating a corpus out of the training and testing data.

CorpusDescription = Corpus(VectorSource(c(eBayTrain$description, eBayTest$description)))

CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removeWords, stopwords("english"), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy=TRUE)

# Now we are ready to convert our corpus to a DocumentTermMatrix, 
# remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, 
# but remember that you can try different numbers!

dtm = DocumentTermMatrix(CorpusDescription)
sparse = removeSparseTerms(dtm, 0.995)
DescriptionWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:

colnames(DescriptionWords) = make.names(colnames(DescriptionWords))

# Now we need to split the observations back into the training 
# set and testing set.

DescriptionWordsTrain = head(DescriptionWords, nrow(eBayTrain))
DescriptionWordsTest = tail(DescriptionWords, nrow(eBayTest))

# Before building models, we want to add back the original 
# variables from our datasets. We'll add back the dependent 
# variable to the training set, and the WordCount variable 
# to both datasets. You might want to add back more variables 
# to use in your model - we'll leave this up to you!

DescriptionWordsTrain$sold = eBayTrain$sold


library(caTools)
spl = sample.split(DescriptionWordsTrain$sold, SplitRatio = 0.7)
Train = subset(DescriptionWordsTrain, spl==TRUE)
Test = subset(DescriptionWordsTrain, spl==FALSE)


#
# Use CART to create a predictive model
#

library(rpart)
library(rpart.plot)

levels(Train$description) = levels(Test$description)

CART1 = rpart(sold ~ . , data = Train)

prp(CART1)

predict_cart = predict(CART1 , newdata = Test)

#make confusion matrix
table(Test$sold , predict_cart > 0.5)

sum(diag(table(Test$sold , predict_cart > 0.5))) / sum(table(Test$sold , predict_cart > 0.5)) 
#0.53

#compare to baseline model of always non-negative
prop.table(table(Test$sold))
#0.53

library(ROCR)

predROCR1 = prediction(predict_cart , Test$sold )
as.numeric(performance(predROCR1,"auc")@y.values)

#
# Use Random Forest to create a predictive model
#

library(randomForest)


#will take a long time because we have 300 variables, RF takes longer than CART
RF = randomForest(sold ~ . , data = Train)

predict_rf = predict(RF , newdata = Test)

#make confusion matrix
table( Test$sold , predict_rf > 0.5 )

sum(diag( table( Test$sold , predict_rf > 0.5 ) )) / sum( table( Test$sold , predict_rf > 0.5 ) )
#0.57

predROCR2 = prediction(predict_rf , Test$sold )
as.numeric(performance(predROCR2,"auc")@y.values)


#
# Building a logistic regression model
#


LogReg = glm(sold ~ . , data = Train , family="binomial")


prediction_LogReg = predict(LogReg, newdata=Test, type="response")
prediction_LogReg_train = predict(LogReg, newdata=Train, type="response")

#Build a confusion matrix (with a threshold of 0.5) 

sum(diag(table( Test$sold ,  prediction_LogReg > 0.5 ))) / sum(table( Test$sold ,  prediction_LogReg > 0.5 ))




predROCR3 = prediction(prediction_LogReg , Test$sold )
as.numeric(performance(predROCR3,"auc")@y.values)
















# Now we can prepare our submission file for Kaggle:

#MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = PredTest2prob)
#write.csv(MySubmission, file.path(substr(default_path,0,51),"Submission2.csv"), row.names=FALSE)

