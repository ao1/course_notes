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
sparse = removeSparseTerms(dtm, 0.95)
DescriptionWords = as.data.frame(as.matrix(sparse))

colnames(DescriptionWords) = make.names(colnames(DescriptionWords))
DescriptionWordsTrain = head(DescriptionWords, nrow(eBayTrain))
DescriptionWordsTest = tail(DescriptionWords, nrow(eBayTest))
DescriptionWordsTrain$sold = eBayTrain$sold

spl = sample.split(DescriptionWordsTrain$sold, SplitRatio = 0.7)
Train = subset(DescriptionWordsTrain, spl==TRUE)
Test = subset(DescriptionWordsTrain, spl==FALSE)


RF = randomForest(sold ~ . , data = Train)
predict_rf = predict(RF , newdata = Test)
sum(diag( table( Test$sold , predict_rf > 0.5 ) )) / sum( table( Test$sold , predict_rf > 0.5 ))


predROCR = prediction(predict_rf , Test$sold )
as.numeric(performance(predROCR,"auc")@y.values)
