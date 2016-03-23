Sys.setlocale("LC_ALL", "C")

if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
} else {
  next
}

eBayTrain = read.csv(file.path(default_path,"eBayiPadTrain.csv"), stringsAsFactors=FALSE)
eBayTest = read.csv(file.path(default_path,"eBayiPadTest.csv"), stringsAsFactors=FALSE)

eBayTrain$storage[eBayTrain$storage == "Unknown"] = 0
eBayTest$storage[eBayTest$storage == "Unknown"] = 0

eBayTest$storage = as.factor(eBayTest$storage)
eBayTrain$storage = as.factor(eBayTrain$storage)

eBayTest$productline = as.factor(eBayTest$productline)
eBayTrain$productline = as.factor(eBayTrain$productline)
eBayTest$condition = as.factor(eBayTest$condition)
eBayTrain$condition = as.factor(eBayTrain$condition)
eBayTest$biddable = as.numeric(eBayTest$biddable)
eBayTrain$biddable = as.numeric(eBayTrain$biddable)

eBayTrain$sold = as.numeric(eBayTrain$sold)

library(caTools)
library(tm)
library(SnowballC)
library(ROCR)
library(randomForest)
library(rpart)
library(rpart.plot)

CorpusDescription = Corpus(VectorSource(c(eBayTrain$description, eBayTest$description)))
CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removeWords, stopwords("english"), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy=TRUE)

dtm = DocumentTermMatrix(CorpusDescription)
sparse = removeSparseTerms(dtm, 0.998)  #0.998 , 0.965
DescriptionWords = as.data.frame(as.matrix(sparse))

colnames(DescriptionWords) = make.names(colnames(DescriptionWords))
DescriptionWordsTrain = head(DescriptionWords, nrow(eBayTrain))
DescriptionWordsTest = tail(DescriptionWords, nrow(eBayTest))

DescriptionWordsTrain$sold = eBayTrain$sold

DescriptionWordsTrain$biddable = eBayTrain$biddable
DescriptionWordsTest$biddable = eBayTest$biddable
DescriptionWordsTrain$startprice = eBayTrain$startprice
DescriptionWordsTest$startprice = eBayTest$startprice
DescriptionWordsTrain$condition = eBayTrain$condition
DescriptionWordsTest$condition = eBayTest$condition
DescriptionWordsTrain$storage = eBayTrain$storage
DescriptionWordsTest$storage = eBayTest$storage
DescriptionWordsTrain$productline = eBayTrain$productline
DescriptionWordsTest$productline = eBayTest$productline


spl1 = sample.split(DescriptionWordsTrain$sold, SplitRatio = 0.9)
Train1 = subset(DescriptionWordsTrain, spl1==TRUE)
Test1 = subset(DescriptionWordsTrain, spl1==FALSE)

spl2 = sample.split(DescriptionWordsTrain$sold, SplitRatio = 0.4)
Train2 = subset(DescriptionWordsTrain, spl2==TRUE)
Test2 = subset(DescriptionWordsTrain, spl2==FALSE)

spl3 = sample.split(DescriptionWordsTrain$sold, SplitRatio = 0.5)
Train3 = subset(DescriptionWordsTrain, spl3==TRUE)
Test3 = subset(DescriptionWordsTrain, spl3==FALSE)

levels(DescriptionWordsTest$productline) = levels(Train1$productline)

#========================================================================================

RF = randomForest(sold ~ . , data = Train1)
prediction_LogReg1 = predict(LogReg1, newdata=Test1, type="response")
prediction_LogReg2 = predict(LogReg1, newdata=Test2, type="response")
prediction_LogReg3 = predict(LogReg1, newdata=Test3, type="response")

sum(diag(table( Test1$sold ,  prediction_LogReg1 > 0.5 ))) / sum(table( Test1$sold ,  prediction_LogReg1 > 0.5 ))
predROCR1 = prediction(prediction_LogReg1 , Test1$sold )
as.numeric(performance(predROCR1,"auc")@y.values)

sum(diag(table( Test2$sold ,  prediction_LogReg2 > 0.5 ))) / sum(table( Test2$sold ,  prediction_LogReg2 > 0.5 ))
predROCR2 = prediction(prediction_LogReg2 , Test2$sold )
as.numeric(performance(predROCR2,"auc")@y.values)

sum(diag(table( Test3$sold ,  prediction_LogReg3 > 0.5 ))) / sum(table( Test3$sold ,  prediction_LogReg3 > 0.5 ))
predROCR3 = prediction(prediction_LogReg3 , Test3$sold )
as.numeric(performance(predROCR3,"auc")@y.values)

prediction_Sub = predict(RF , newdata = DescriptionWordsTest)
#kaggle: 0.79347

