Sys.setlocale("LC_ALL", "C")

if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
} else {
  next
}

eBayTrain = read.csv(file.path(default_path,"eBayiPadTrain.csv"), stringsAsFactors=TRUE, na.strings = c("Unknown", "None","NA"))
eBayTest = read.csv(file.path(default_path,"eBayiPadTest.csv"), stringsAsFactors=TRUE, na.strings = c("Unknown", "None","NA"))

#install.packages("mice")

library(mice)
simple = eBayTrain[c("storage","carrier","cellular","productline")]
imputed = complete(mice(simple))
eBayTrain$storage = imputed$storage
eBayTrain$carrier = imputed$carrier
eBayTrain$cellular = imputed$cellular
eBayTrain$color = imputed$color
eBayTrain$productline = imputed$productline

library(caTools)
library(tm)
library(SnowballC)
library(ROCR)
library(randomForest)
library(rpart)
library(rpart.plot)

CorpusDescription = Corpus(VectorSource(c(eBayTrain$description, eBayTest$description, 
                                          eBayTest$condition, eBayTrain$condition)))
CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removeWords, stopwords("english"), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy=TRUE)

dtm = DocumentTermMatrix(CorpusDescription)
sparse = removeSparseTerms(dtm, 0.95)
DescriptionWords = as.data.frame(as.matrix(sparse))

colnames(DescriptionWords) = make.names(colnames(DescriptionWords))
DescriptionWordsTrain = head(DescriptionWords, nrow(eBayTrain))
DescriptionWordsTest = tail(DescriptionWords, nrow(eBayTest)) #test on this before submitting
DescriptionWordsTrain$sold = eBayTrain$sold

DescriptionWordsTrain$biddable = eBayTrain$biddable
DescriptionWordsTest$biddable = eBayTest$biddable
DescriptionWordsTrain$startprice = eBayTrain$startprice
DescriptionWordsTest$startprice = eBayTest$startprice
DescriptionWordsTrain$storage = eBayTrain$storage
DescriptionWordsTest$storage = eBayTest$storage
DescriptionWordsTrain$productline = eBayTrain$productline
DescriptionWordsTest$productline = eBayTest$productline

spl1 = sample.split(DescriptionWordsTrain$sold, SplitRatio = 0.9)
Train1 = subset(DescriptionWordsTrain, spl1==TRUE)
Test1 = subset(DescriptionWordsTrain, spl1==FALSE)

spl2 = sample.split(DescriptionWordsTrain$sold, SplitRatio = 0.2)
Train2 = subset(DescriptionWordsTrain, spl2==TRUE)
Test2 = subset(DescriptionWordsTrain, spl2==FALSE)

spl3 = sample.split(DescriptionWordsTrain$sold, SplitRatio = 0.3)
Train3 = subset(DescriptionWordsTrain, spl3==TRUE)
Test3 = subset(DescriptionWordsTrain, spl3==FALSE)

spl4 = sample.split(DescriptionWordsTrain$sold, SplitRatio = 0.4)
Train4 = subset(DescriptionWordsTrain, spl4==TRUE)
Test4 = subset(DescriptionWordsTrain, spl4==FALSE)

#========================================================================================

LogReg1 = glm(sold ~ . , data = Train1 , family="binomial")
prediction_LogReg2 = predict(LogReg1, newdata=Test2, type="response")
prediction_LogReg3 = predict(LogReg1, newdata=Test3, type="response")
prediction_LogReg4 = predict(LogReg1, newdata=Test4, type="response")

# sum(diag(table( Test1$sold ,  prediction_LogReg1 > 0.5 ))) / sum(table( Test1$sold ,  prediction_LogReg1 > 0.5 ))
# predROCR1 = prediction(prediction_LogReg1 , Test1$sold )
# as.numeric(performance(predROCR1,"auc")@y.values)

sum(diag(table( Test2$sold ,  prediction_LogReg2 > 0.5 ))) / sum(table( Test2$sold ,  prediction_LogReg2 > 0.5 ))
predROCR2 = prediction(prediction_LogReg2 , Test2$sold )
as.numeric(performance(predROCR2,"auc")@y.values)

sum(diag(table( Test3$sold ,  prediction_LogReg3 > 0.5 ))) / sum(table( Test3$sold ,  prediction_LogReg3 > 0.5 ))
predROCR3 = prediction(prediction_LogReg3 , Test3$sold )
as.numeric(performance(predROCR3,"auc")@y.values)

sum(diag(table( Test4$sold ,  prediction_LogReg4 > 0.5 ))) / sum(table( Test4$sold ,  prediction_LogReg4 > 0.5 ))
predROCR4 = prediction(prediction_LogReg4 , Test4$sold )
as.numeric(performance(predROCR4,"auc")@y.values)

#prediction_Sub = predict(LogReg1 , newdata = DescriptionWordsTest, type="response")