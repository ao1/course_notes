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


library(caTools)
spl = sample.split(eBayTrain$sold, SplitRatio = 0.7)
Train = subset(eBayTrain, spl==TRUE)
Test = subset(eBayTrain, spl==FALSE)


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
sparse = removeSparseTerms(dtm, 0.99)
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






















# Now we can prepare our submission file for Kaggle:

MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = PredTest2prob)

write.csv(MySubmission, file.path(substr(default_path,0,51),"Submission2.csv"), row.names=FALSE)

